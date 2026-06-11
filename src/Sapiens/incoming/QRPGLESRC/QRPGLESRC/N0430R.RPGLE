     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE N0430R - CASH POSTING
     H*****************************************************************
     H* PROGRAM DESCRIPTION: POST CASH TO ONE CASE & INVOICE ---      *
     H*                         WRITES A/R RECORDS TO MMARP.          *
     H*                                                               *
     H* Called by: N0419R - A/R INQUIRY - COMPANY LEVEL               *
     H*            N0420R - A/R INQUIRY - CASE LEVEL                  *
     H* Calls    : NONE                                               *
     H*                                                               *
     H* Created: 04/07/00  C:62  Project: AR001                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FN0430F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FFTABL     IF   E           K DISK
     FNMARL5    IF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     FNMIMP     IF   E             DISK
     FNMARP     O    E           K DISK
     FFTACP     IF   E             DISK
     F                                     RENAME(PAC01:PAC02)
     F*

     D*===============================================================*
     D*     EXTENSION SECTION                                         *
     D*===============================================================*
     D*
     D* DEFINE ARRAYS
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  SY                     1     10
     D                                     DIM(10) ASCEND                         SYSTEM ID
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
     D                 DS
     D  DC                     1    300
     D                                     DIM(10) ASCEND                         DESCRIPTION
     D  SCDC01                 1     30
     D  SCDC02                31     60
     D  SCDC03                61     90
     D  SCDC04                91    120
     D  SCDC05               121    150
     D  SCDC06               151    180
     D  SCDC07               181    210
     D  SCDC08               211    240
     D  SCDC09               241    270
     D  SCDC10               271    300
     D                 DS
     D  IA                     1     90  2
     D                                     DIM(10) ASCEND                         INVOICED AMT
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
     D                 DS
     D  OA                     1     90  2
     D                                     DIM(10) ASCEND                         OPEN AMT
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
     D                 DS
     D  DA                     1     90  2
     D                                     DIM(10) ASCEND                         DISBURSD AMT
     D  SCDA01                 1      9  2
     D  SCDA02                10     18  2
     D  SCDA03                19     27  2
     D  SCDA04                28     36  2
     D  SCDA05                37     45  2
     D  SCDA06                46     54  2
     D  SCDA07                55     63  2
     D  SCDA08                64     72  2
     D  SCDA09                73     81  2
     D  SCDA10                82     90  2
     D QQPRDS          C                   CONST('              Cash P-
     D                                     osting              ')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    ARSUB#        XXSUB#
     C     *LIKE         DEFINE    ARDT          XXPODT
     C*
     C*
     C*   KEY LIST FOR THE COMPANY FILE  (SMCOL)
     C*
     C     COKEY         KLIST
     C                   KFLD                    IMCO#
     C*
     C*  USER PARAMETER FILE  (FMUPL)
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  TABLE FILE (FTABL)
     C*
     C     TBKEY         KLIST
     C                   KFLD                    IMCO#
     C                   KFLD                    KKKEY            10
     C*
     C*  A/R FILE   (NMARL5)
     C*
     C     ARKEY         KLIST
     C                   KFLD                    IMCO#
     C                   KFLD                    IMSUB#
     C                   KFLD                    IMFND
     C                   KFLD                    IMFYR
     C                   KFLD                    IMCASE
     C                   KFLD                    IMINV#
     C*
     C     *ENTRY        PLIST
     C                   PARM      *BLANKS       @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          NMIMP
     C                   PARM                    @@IAMT            9 2
     C                   PARM                    @@PAMT            9 2
     C                   PARM                    @@AAMT            9 2
     C                   PARM                    @@TDUE            9 2
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
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     @@RRN         CHAIN     PIM01                              99
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     @@CALL        COMP      '1'                                    09
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      IMCO#         #1CO#             3
     C                   MOVE      IMSUB#        #1SUB#            5
     C                   MOVE      IMFND         #1FND             3
     C                   MOVE      IMFYR         #1FYR             3
     C                   MOVE      IMCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     SCHD1         PARM      *BLANK        ##L1             78
     C     SCHD2         PARM      *BLANK        ##L2             78
     C*
     C                   Z-ADD     IMINV#        SCINV#
     C*
     C                   Z-ADD     IMTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C*
     C                   Z-ADD     @@IAMT        SCIAMT
     C                   Z-ADD     @@PAMT        SCPAMT
     C                   Z-ADD     @@AAMT        SCAAMT
     C                   Z-ADD     @@TDUE        SCTDUE
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   Z-ADD     *ZEROS        SCCAMT
     C                   Z-ADD     *ZEROS        SCCK#
     C                   MOVEL     *BLANKS       SCTRCD
     C                   MOVEL     *BLANKS       SCTCDS
     C                   MOVEL     *BLANKS       SCTCD2
     C                   Z-ADD     DSDATE        SCPODT
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
     C                   EXFMT     SCR02
     C                   END
     C****************************************************************
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C**********************************
     C*   F3 = EXIT                    *
     C**********************************
     C     *INKC         IFEQ      '1'
     C*
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                  ENDIF *INKC
     C**********************************
     C*   F5 = RESTART                 *
     C**********************************
     C     *INKE         IFEQ      '1'
     C*
     C                   GOTO      SCR01T
     C*
     C                   ENDIF                                                  ENDIF *INKE
     C/SPACE 3
     C*
     C**********************************
     C*   F10 = SEARCH                 *
     C**********************************
     C     *INKJ         IFEQ      '1'
     C*
     C     HFLD          IFEQ      'SCTRCD'
     C*
     C                   CALL      'N2305R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@RRN             9 0
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         CABEQ     *ON           STRT                     35
     C*
     C     @@RRN         CHAIN     PAC02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  ACKEY         SCTRCD
     C                   MOVEL(P)  ACDESC        SCTCDS
     C                   MOVEL(P)  ACDESC        SCTCD2
     C                   MOVE      *ON           *IN27
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN35
     C*
     C                   ENDIF                                                  IFEQ *IN60
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  HFLD SCTRCD
     C*
     C     HFLD          IFEQ      'SCINV#'
     C     HFLD          OREQ      'SCCAMT'
     C     HFLD          OREQ      'SCCK#'
     C     HFLD          OREQ      'SCPODT'
     C*
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  HFLD SCINV#
     C*
     C                   ENDIF                                                  IFEQ INKJ
     C**********************************
     C*   F12 = PREVIOUS               *
     C**********************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     *ZEROS        SCIAMT
     C                   Z-ADD     *ZEROS        SCOAMT
     C                   Z-ADD     *ZEROS        SCDAMT
     C                   Z-ADD     *ZEROS        XXIAMT
     C                   Z-ADD     *ZEROS        XXOAMT
     C                   Z-ADD     *ZEROS        XXBAMT
     C                   MOVEL     *BLANKS       SY
     C                   Z-ADD     *ZEROS        IA
     C                   Z-ADD     *ZEROS        OA
     C                   Z-ADD     *ZEROS        DA
     C*
     C                   ENDIF                                                  ENDIF SCR02
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  ENDIF *INKL
     C**********************************
     C*   PROCESS HELP REQUESTS        *
     C**********************************
     C     *IN10         IFEQ      '1'
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  ENDIF *IN10
     C*
     C                   ENDIF                                                  END CMDKEY
     C****************************************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C***************************
     C* PROCESS REQUEST SCREEN  *
     C***************************
     C     SCR01T        TAG
     C*
     C                   Z-ADD     *ZEROS        SCDAMT
     C                   Z-ADD     *ZEROS        SCOAMT
     C                   Z-ADD     *ZEROS        SCIAMT
     C                   Z-ADD     *ZEROS        XXIAMT
     C                   Z-ADD     *ZEROS        XXOAMT
     C                   Z-ADD     *ZEROS        XXBAMT
     C                   MOVEL     *BLANKS       SY
     C                   Z-ADD     *ZEROS        IA
     C                   Z-ADD     *ZEROS        OA
     C                   Z-ADD     *ZEROS        DA
     C*
     C* BEGIN EDIT CHECKING
     C*
     C*   VALIDATE CHECK AMOUNT
     C*
     C     SCCAMT        IFEQ      *ZERO
     C*
     C                   MOVE      '1'           *IN23                          INVALID AMT
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  ENDIF LE
     C*
     C*   VALIDATE CHECK NUMBER
     C*
     C     SCCK#         IFLE      *ZERO
     C*
     C                   MOVE      '1'           *IN24                          INVALID CHK#
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  ENDIF LE
     C*
     C*   VALIDATE TRANSACTION CODE
     C*
     C                   MOVEL(P)  SCTRCD        KKKEY
     C*
     C     TBKEY         CHAIN     PAC01                              99
     C*
     C     *IN99         IFEQ      '1'
     C*
     C                   MOVE      '1'           *IN22                          INVALID CODE
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  ACDESC        SCTCDS
     C                   MOVEL(P)  ACDESC        SCTCD2
     C*
     C                   ENDIF                                                  ENDIF *IN60
     C*
     C     ACTYPE        IFNE      'P'
     C     ACTYPE        ANDNE     'N'
     C*
     C                   MOVE      '1'           *IN22                          INVALID CODE
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVEL     ACDESC        XXDESC           30            TRANS DESC
     C*
     C                   ENDIF                                                  ENDNE 'P'
     C*
     C*   VALIDATE POST DATE
     C*
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   MOVE      '1'           *IN25                          INVALID DATE
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  ENDIF SRERR
     C*
     C* POSTING DATE MUST BE GREATER THAN INVOICE DATE.
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C*
     C     XXPODT        IFLT      IMTRDT
     C*
     C                   MOVE      '1'           *IN26                          INVALID DATE
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  ENDIF SRERR
     C*
     C*  CALCULATE SYSTEM INVOICE & DISBURSED AMOUNTS
     C*
     C     ARKEY         CHAIN     PAR02                              50
     C*
     C     *IN50         IFEQ      '0'
     C*
     C                   Z-ADD     SCCAMT        XXBAMT            9 2          BAL CHK AMT
     C                   MOVE      ARSYS         XXSYS             1            HOLD SYS ID
     C*
     C                   ENDIF                                                  ENDIF *IN60
     C*
     C                   Z-ADD     1             X                              ARRAY INDEX
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*  DETERMINE IF SYSTEM CODE CHANGES
     C*
     C     ARSYS         IFNE      XXSYS
     C*
     C                   MOVE      XXSYS         SY(X)                          SYSTEM ID
     C                   MOVEL     XXDESC        DC(X)                          DESCRIPTION
     C                   Z-ADD     XXIAMT        IA(X)                          INVOICE AMT
     C                   Z-ADD     XXOAMT        OA(X)                          OPEN AMT
     C*
     C     XXOAMT        IFGT      *ZERO
     C     XXOAMT        IFGE      XXBAMT
     C*
     C                   Z-ADD     XXBAMT        DA(X)                          DISBURSE AMT
     C                   Z-ADD     *ZERO         XXBAMT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     XXOAMT        DA(X)                          DISBURSE AMT
     C     XXBAMT        SUB       XXOAMT        XXBAMT
     C*
     C                   ENDIF                                                  ENDGE XXBAMT
     C                   ENDIF                                                  ENDGT *ZERO
     C*
     C                   MOVE      ARSYS         XXSYS                          HOLD SYS ID
     C                   Z-ADD     *ZERO         XXIAMT                         SYS INV AMT
     C                   Z-ADD     *ZERO         XXOAMT                         SYS OPN AMT
     C                   ADD       1             X
     C*
     C                   ENDIF                                                  ENDNE XXSYS
     C*
     C*  CALCULATE INVOICE AMOUNT
     C*
     C     ARTRTP        IFEQ      'I'
     C*
     C                   ADD       ARAMT         XXIAMT            9 2          SYS INV AMT
     C                   ADD       ARAMT         SCIAMT                         TOT INV AMT
     C*
     C                   ENDIF                                                  ENDIF 'I'
     C*
     C*  CALCULATE OPEN AMOUNT
     C*
     C     ARTRTP        IFEQ      'P'
     C*
     C                   SUB       ARAMT         XXOAMT            9 2          SYS OPEN AMT
     C                   SUB       ARAMT         SCOAMT                         TOT OPEN AMT
     C*
     C                   ELSE
     C*
     C                   ADD       ARAMT         XXOAMT
     C                   ADD       ARAMT         SCOAMT
     C*
     C                   ENDIF                                                  ENDIF 'P'
     C*
     C     ARKEY         READE     PAR02                                  50
     C*
     C                   ENDDO                                                  END DOWEQ
     C*
     C*  END OF FILE OR KEY HAS CHANGED - OUTPUT LAST SYSTEM'S AMOUNTS
     C*
     C                   MOVE      XXSYS         SY(X)                          SYSTEM ID
     C                   MOVEL     XXDESC        DC(X)                          DESCRIPTION
     C                   Z-ADD     XXIAMT        IA(X)                          INVOICE AMT
     C                   Z-ADD     XXOAMT        OA(X)                          OPEN AMT
     C*
     C     XXOAMT        IFGT      *ZERO
     C     XXOAMT        IFGE      XXBAMT
     C*
     C                   Z-ADD     XXBAMT        DA(X)                          DISBURSE AMT
     C                   Z-ADD     *ZERO         XXBAMT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     XXOAMT        DA(X)                          DISBURSE AMT
     C     XXBAMT        SUB       XXOAMT        XXBAMT
     C*
     C                   ENDIF                                                  ENDGE XXBAMT
     C                   ENDIF                                                  ENDGT *ZERO
     C*
     C*  CALCULATE SUM OF DISBURSED AMOUNTS
     C*
     C                   Z-ADD     1             X                 2 0
     C*
     C     X             DOWLE     10
     C*
     C                   ADD       DA(X)         SCDAMT            9 2          SUM OF DISB
     C                   ADD       1             X
     C*
     C                   ENDDO                                                  END DOWLE 10
     C*
     C*  IF OPEN AMOUNT DOES NOT EQUAL CHECK AMOUNT - DISPLAY WARNING
     C*
     C     SCOAMT        IFNE      SCCAMT
     C*
     C                   MOVE      '1'           *IN25                          WARNING
     C*
     C                   ELSE
     C*
     C                   MOVE      '0'           *IN25
     C*
     C                   ENDIF                                                  ENDNE SCCAMT
     C*
     C                   DO        10            Z                 5 0
     C*
     C     *BLANKS       LOOKUP    SY(Z)                                  60
     C*
     C     *IN60         IFEQ      '1'
     C*
     C     Z             ADD       30            A                 2 0
     C                   MOVE      '1'           *IN(A)
     C                   MOVE      '1'           *IN41
     C*
     C                   ENDIF                                                  END  *IN60
     C                   ENDDO                                                  END  DO
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C***************************
     C* PROCESS REQUEST SCREEN  *
     C***************************
     C     SCR02T        TAG
     C*
     C* BEGIN EDIT CHECKING
     C*
     C*  IF OPEN AMOUNT DOES NOT EQUAL CHECK AMOUNT - DISPLAY WARNING
     C*
     C     SCOAMT        IFNE      SCCAMT
     C*
     C                   MOVE      '1'           *IN25                          WARNING
     C*
     C                   ELSE
     C*
     C                   MOVE      '0'           *IN25
     C*
     C                   ENDIF                                                  ENDNE SCCAMT
     C*
     C*  VALIDATE THAT THE DESCRIPTIONS ARE NOT BLANK
     C*
     C                   Z-ADD     1             X                 2 0
     C*
     C     X             DOWLE     10
     C     SY(X)         ANDNE     *BLANKS
     C*
     C     X             ADD       30            A                 2 0
     C*
     C     DC(X)         IFEQ      *BLANKS
     C*
     C                   MOVE      '1'           *IN27                          NEEDS DESC
     C                   MOVE      '1'           *IN(A)
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  ENDIF *BLNKS
     C*
     C                   ADD       1             X
     C*
     C                   ENDDO                                                  END DOWLE 10
     C*
     C*  VALIDATE THAT SUM OF DISBURSED AMOUNTS EQUALS CHECK AMOUNT
     C*
     C                   Z-ADD     *ZERO         SCDAMT
     C                   Z-ADD     1             X                 2 0
     C*
     C     X             DOWLE     10
     C*
     C                   ADD       DA(X)         SCDAMT            9 2          SUM OF DISB
     C                   ADD       1             X
     C*
     C                   ENDDO                                                  END DOWLE 10
     C*
     C     SCCAMT        IFNE      SCDAMT
     C*
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  ENDNE SCDAMT
     C*
     C*
     C*  ALL EDITS HAVE BEEN PASSED:  WRITE RECORDS TO A/R FILE
     C*
     C*  GET A/R TRANSACTION TYPE
     C*
     C                   MOVEL(P)  SCTRCD        KKKEY
     C*
     C     TBKEY         CHAIN     PAC01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ACTYPE        ARTRTP                         TRAN TYPE
     C*
     C                   ENDIF                                                  ENDIF *IN60
     C*
     C*  MOVE SCREEN FIELDS TO FILE FIELDS FOR NMARP
     C*
     C                   Z-ADD     IMCO#         ARCO#                          COMPANY #
     C                   Z-ADD     IMSUB#        ARSUB#                          10/18/89 CHG
     C                   Z-ADD     IMFND         ARFND                          GROUP
     C                   Z-ADD     IMFYR         ARFYR                          FUND YEAR
     C                   Z-ADD     IMCASE        ARCASE                         CASE
     C                   MOVEL     SCTRCD        ARTRCD                         TRANS CODE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARTRDT
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARPODT
     C*
     C                   MOVE      SCCK#         ARCK#                          CHECK NUMBER
     C                   MOVE      Q1USER        ARUSER                         USER
     C                   MOVE      Q1PGM         ARPGM                          PROGRAM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   Z-ADD     DSTIME        ARTIME                         TIME MAINT
     C                   MOVE      SCINV#        ARINV#                         INVOICE #
     C                   Z-ADD     *ZEROS        AREOB#                         EOB #
     C                   MOVE      *BLANKS       ARSTAT                         STATUS
     C*
     C                   Z-ADD     1             X                 2 0
     C*
     C     X             DOWLE     10
     C     SY(X)         ANDNE     *BLANKS
     C*
     C     DA(X)         IFNE      *ZERO
     C*
     C                   MOVEL     SY(X)         ARSYS                          SYSTEM CODE
     C                   MOVEL     DC(X)         ARDESC                         DESCRIPTION
     C                   MOVE      DA(X)         ARAMT                          DISB CHK AMT
     C*
     C                   WRITE     PAR01
     C*
     C                   ENDIF                                                  ENDNE *ZERO
     C*
     C                   ADD       1             X
     C*
     C                   ENDDO                                                  END DOWLE
     C*
     C                   Z-ADD     *ZEROS        SCINV#
     C                   MOVEL     *BLANKS       SCTRCD
     C                   MOVEL     *BLANKS       SCTCDS
     C                   Z-ADD     *ZEROS        SCIAMT
     C                   Z-ADD     *ZEROS        SCOAMT
     C                   Z-ADD     *ZEROS        SCDAMT
     C                   Z-ADD     *ZEROS        SCCAMT
     C                   Z-ADD     *ZEROS        SCCK#
     C                   Z-ADD     *ZEROS        XXIAMT
     C                   Z-ADD     *ZEROS        XXOAMT
     C                   Z-ADD     *ZEROS        XXBAMT
     C                   MOVEL     *BLANKS       SY
     C                   Z-ADD     *ZEROS        IA
     C                   Z-ADD     *ZEROS        OA
     C                   Z-ADD     *ZEROS        DA
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TERM
     C*
     C***********************
     C*  END THE PROGRAM    *
     C***********************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
