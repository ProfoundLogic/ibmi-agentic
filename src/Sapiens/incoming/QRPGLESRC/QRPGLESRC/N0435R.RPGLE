     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE N0435R - ADJUSTMENT POSTING
     H*****************************************************************
     H* PROGRAM DESCRIPTION: ADJUSTMENT POSTING                       *
     H*                      WRITES A/R RECORDS TO MMARP.             *
     H*                                                               *
     H* Called by: N0425R - WORK WITH INVOICE DETAIL                  *
     H* Calls    : NONE                                               *
     H*                                                               *
     H* Created: 04/07/00  C:62  Project: AR001                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  08/24/00 C:62 PROJ:MCMNT                                     *
     H*                UPDATE HEADER AMOUNTS.                         *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FN0435F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FFTABL     IF   E           K DISK
     FNMIMP     IF   E             DISK
     FNMARP     O    E             DISK
     F**** BEGIN ADD *** 08/24/00 *************************************
     FNMARL6    IF   E           K DISK
     F                                     RENAME(PAR01:PAR02)
     F**** END   ADD *** 08/24/00 *************************************
     FFTACP     IF   E             DISK
     F                                     RENAME(PAC01:PAC02)
     F*

     D*===============================================================*
     D*     EXTENSION SECTION                                         *
     D*===============================================================*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D QQPRDS          C                   CONST('           Adjustmen-
     D                                     t Posting           ')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    ARDT          XXPODT
     C     *LIKE         DEFINE    AREOB#        XXEOB#
     C     *LIKE         DEFINE    ARRCN         XXRCN
     C**** BEGIN ADD *** 08/24/00 *************************************
     C     ARKY          KLIST
     C                   KFLD                    IMCO#
     C                   KFLD                    IMSUB#
     C                   KFLD                    IMFND
     C                   KFLD                    IMFYR
     C                   KFLD                    IMCASE
     C                   KFLD                    IMINV#
     C**** END   ADD *** 08/24/00 *************************************
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
     C     *ENTRY        PLIST
     C                   PARM      *BLANKS       @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          NMIMP
     C                   PARM                    @@EOB#            9 0
     C                   PARM                    @@RCN             3 0
     C                   PARM                    @@SYS             1
     C**** BEGIN ADD *** 08/24/00 *************************************
     C***********          PARM           @@IAMT  92
     C***********          PARM           @@PAMT  92
     C***********          PARM           @@AAMT  92
     C***********          PARM           @@TDUE  92
     C**** END   ADD *** 08/24/00 *************************************
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
     C                   Z-ADD     @@EOB#        SCEOB#
     C                   Z-ADD     @@RCN         SCRCN
     C*
     C                   Z-ADD     IMTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTRDT
     C*
     C**** BEGIN DEL *** 08/24/00 *************************************
     C***********          Z-ADD@@IAMT    SCIAMT
     C***********          Z-ADD@@PAMT    SCPAMT
     C***********          Z-ADD@@AAMT    SCAAMT
     C***********          Z-ADD@@TDUE    SCTDUE
     C**** END   DEL *** 08/24/00 *************************************
     C**** BEGIN ADD *** 08/24/00 *************************************
     C                   EXSR      SRAMT
     C**** END   ADD *** 08/24/00 *************************************
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   Z-ADD     *ZEROS        SCCAMT
     C                   MOVEL     *BLANKS       SCTRCD
     C                   MOVEL     *BLANKS       SCTCDS
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
     C     HFLD          IFEQ      'SCCAMT'
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
     C                   Z-ADD     *ZEROS        SCCAMT
     C                   MOVEL     *BLANKS       SCTRCD
     C                   MOVEL     *BLANKS       SCTCDS
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                  ENDIF SCR02
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
     C*
     C***************************
     C* PROCESS REQUEST SCREEN  *
     C***************************
     C     SCR01T        TAG
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
     C                   ENDIF
     C*
     C     ACTYPE        IFEQ      'P'
     C*
     C                   MOVE      '1'           *IN22                          INVALID CODE
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     SCTCDS        IFEQ      *BLANKS
     C*
     C                   MOVEL(P)  ACDESC        SCTCDS
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  ENDNE 'P'
     C*
     C*   MANUAL INVOICE NOT ALLOWED
     C*
     C     SCTRCD        IFEQ      'MI'
     C*
     C                   SETON                                        31
     C                   GOTO      STRT
     C*
     C                   ENDIF
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
     C*  ALL EDITS HAVE BEEN PASSED:  WRITE RECORDS TO A/R FILE
     C*
     C                   CLEAR                   PAR01
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
     C                   Z-ADD     @@EOB#        AREOB#
     C                   Z-ADD     @@RCN         ARRCN
     C                   Z-ADD     *ZEROS        ARCK#                          CHECK NUMBER
     C                   MOVEL(P)  Q1USER        ARUSER                         USER
     C                   MOVEL(P)  Q1PGM         ARPGM                          PROGRAM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ARDT
     C*
     C                   Z-ADD     DSTIME        ARTIME                         TIME MAINT
     C                   MOVE      SCINV#        ARINV#                         INVOICE #
     C                   MOVE      *BLANKS       ARSTAT                         STATUS
     C                   MOVEL     SCTCDS        ARDESC                         DESCRIPTION
     C                   Z-ADD     SCCAMT        ARAMT                          DISB CHK AMT
     C                   MOVEL     @@SYS         ARSYS
     C*
     C                   WRITE     PAR01
     C*
     C                   MOVEL     *BLANKS       SCTRCD
     C                   MOVEL     *BLANKS       SCTCDS
     C                   Z-ADD     *ZEROS        SCCAMT
     C                   Z-ADD     DSDATE        SCPODT
     C**** BEGIN ADD *** 08/24/00 *************************************
     C                   EXSR      SRAMT
     C**** END   ADD *** 08/24/00 *************************************
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C***********************
     C*  END THE PROGRAM    *
     C***********************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C**** BEGIN ADD *** 08/24/00 *************************************
     C*================================================================
     C* SRAMT - Calculate the header amounts.
     C*================================================================
     C     SRAMT         BEGSR
     C*
     C                   Z-ADD     *ZEROS        SCIAMT
     C                   Z-ADD     *ZEROS        SCPAMT
     C                   Z-ADD     *ZEROS        SCAAMT
     C*
     C     ARKY          CHAIN     PAR02                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C                   SELECT
     C*
     C     ARTRTP        WHENEQ    'P'
     C                   ADD       ARAMT         SCPAMT
     C*
     C     ARTRTP        WHENEQ    'A'
     C                   ADD       ARAMT         SCAAMT
     C*
     C     ARTRTP        WHENEQ    'I'
     C                   ADD       ARAMT         SCIAMT
     C*
     C                   ENDSL
     C*
     C     ARKY          READE     PAR02                                  99
     C*
     C                   ENDDO
     C*
     C     SCIAMT        SUB       SCPAMT        SCTDUE
     C                   ADD       SCAAMT        SCTDUE
     C*
     C                   ENDSR
     C**** END   ADD *** 08/24/00 *************************************
