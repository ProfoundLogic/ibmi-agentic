     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H/TITLE N0440R - MANUAL INVOICE POSTING
     H*****************************************************************
     H* PROGRAM DESCRIPTION: MANUAL INVOICE POSTING - WRITES ONLY     *
     H*                      MANUAL INVOICE A/R RECORDS TO NMARP.     *
     H*                                                               *
     H* CALLED FROM: MENU                                             *
     H*              N0419R - A/R INQUIRY - COMPANY LEVEL             *
     H*              N0420R - A/R INQUIRY - CASE LEVEL                *
     H*                                                               *
     H* Created: 04/07/00  C:62  Project: AR001                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FN0440F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FFTABL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     FFMC1L     UF   E           K DISK
     FFMCMP     IF   E             DISK
     FFMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FNMARP     O    E             DISK
     FNMIMP     O    E             DISK
     F*

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************

     D*===============================================================*
     D*     EXTENSION SECTION                                         *
     D*===============================================================*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  SCSS#                  1     11
     D  X2SS#1                 1      3
     D  X2HYP1                 4      4
     D  X2SS#2                 5      6
     D  X2HYP2                 7      7
     D  X2SS#3                 8     11
     D                 DS
     D  XXSS#                  1      9
     D  XXSS#1                 1      3
     D  XXSS#2                 4      5
     D  XXSS#3                 6      9
     D*
     D QQPRDS          C                   CONST('         Manual Invo-
     D                                     ice Posting         ')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    ARDT          XXPODT
     C     *LIKE         DEFINE    AREOB#        XXEOB#
     C     *LIKE         DEFINE    ARRCN         XXRCN
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKGKEY
     C*
     C*   KEY LIST FOR THE COMPANY AUXILIARY FILE
     C*
     C     C1KY          KLIST
     C                   KFLD                    ##CO#
     C*
     C*   KEY LIST FOR THE CASE MASTER FILE  (FMCML1)
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C*   KEY LIST FOR THE COMPANY FILE  (SMCOL)
     C*
     C     COKEY         KLIST
     C                   KFLD                    ##CO#
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE  (FTGTL)
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKGKEY
     C*
     C*  TABLE FILE (FTABL)
     C*
     C     TBKEY         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY            10
     C*
     C     *ENTRY        PLIST
     C                   PARM      *BLANKS       @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          FMCMP
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
     C                   CALL      'FA013R'
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     @@RRN         IFEQ      *ZEROS
     C*
     C                   SETOFF                                       16
     C                   Z-ADD     *ZEROS        SCSUB#
     C                   Z-ADD     *ZEROS        SCFND
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   MOVEL     *BLANKS       SCSS#
     C                   Z-ADD     *ZEROS        SCACDT
     C*
     C                   ELSE
     C*
     C                   SETON                                        16
     C*
     C     @@RRN         CHAIN     PCM01                              99
     C*
     C                   Z-ADD     CMSUB#        SCSUB#
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     CMSS#         XXSS#
     C*
     C***                MOVEL     XXSS#1        X2SS#1
     C***                MOVEL     '-'           X2HYP1
     C***                MOVEL     XXSS#2        X2SS#2
     C***                MOVEL     '-'           X2HYP2
     C***                MOVEL     XXSS#3        X2SS#3
      /Free
       xxSS# = MaskData('SSN':'CMSS#':CMSS#);
       scSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   ENDIF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCCONM
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C     @@CALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL     *BLANKS       SCSYS
     C                   MOVEL     *BLANKS       SCSYSD
     C                   MOVEL     *BLANKS       SCSYS
     C                   Z-ADD     *ZEROS        SCCAMT
     C                   MOVEL     'MI'          SCTRCD
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
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCSYS'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'SYS'         ##TBID            4
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        37
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SCSYS
     C                   MOVEL(P)  GTDESC        SCSYSD
     C*
     C                   SETON                                        32
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCSUB#'
     C     HFLD          OREQ      'SCFND'
     C     HFLD          OREQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   CALL      'F0269R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      SCSUB#        #1SUB#            5 0
     C                   PARM      SCFND         #1FND             3 0
     C                   PARM      SCFYR         #1FYR             3 0
     C                   PARM      *ZEROS        #1CASE            7 0
     C     XXSS#         PARM      *BLANKS       #1SS#             9
     C                   PARM      *ZEROS        #1ACDT            8 0
     C                   PARM      *BLANKS       #1RTCD            1
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C*
     C     #1CASE        IFEQ      *ZEROS
     C*
     C                   SETON                                        31
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     #1SUB#        SCSUB#
     C                   Z-ADD     #1FND         SCFND
     C                   Z-ADD     #1FYR         SCFYR
     C                   Z-ADD     #1CASE        SCCASE
     C                   Z-ADD     #1ACDT        SCACDT
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     XXSS#1        X2SS#1
     C***                MOVEL     '-'           X2HYP1
     C***                MOVEL     XXSS#2        X2SS#2
     C***                MOVEL     '-'           X2HYP2
     C***                MOVEL     XXSS#3        X2SS#3
       scSS# = DisplaySS#(MaskData('SSN':'XXSS#':XXSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   SETON                                        37
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*
     C                   OTHER
     C*
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
     C*
     C                   ENDSL                                                  HFLD SCINV#
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
     C                   MOVEL     'MI'          SCTRCD
     C                   MOVEL     *BLANKS       SCTCDS
     C                   MOVEL     *BLANKS       SCSS#
     C                   Z-ADD     *ZEROS        SCACDT
     C                   MOVEL     *BLANKS       SCSYS
     C                   MOVEL     *BLANKS       SCSYSD
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
     C* CLAIM MUST BE VALID
     C*
     C     CMKY          CHAIN     PCM02                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        33
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C* SYSTEM TYPE MUST BE VALID
     C*
     C                   MOVEL(P)  'SYS'         KKTBID
     C                   MOVEL(P)  SCSYS         KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        38
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  GTDESC        SCSYSD
     C*
     C                   ENDIF
     C*
     C*   VALIDATE CHECK AMOUNT
     C*
     C     SCCAMT        IFEQ      *ZERO
     C*
     C                   SETON                                        34
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  ENDIF LE
     C*
     C*   GET DESCRIPTION
     C*
     C                   MOVEL(P)  SCTRCD        KKKEY
     C*
     C     TBKEY         CHAIN     PAC01                              99
     C*
     C     SCTCDS        IFEQ      *BLANKS
     C     *IN99         ANDEQ     *OFF
     C*
     C                   MOVEL(P)  ACDESC        SCTCDS
     C*
     C                   ENDIF
     C*
     C*   VALIDATE POST DATE
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        35
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
     C                   Z-ADD     ##CO#         ARCO#                          COMPANY #
     C                   Z-ADD     SCSUB#        ARSUB#                          10/18/89 CHG
     C                   Z-ADD     SCFND         ARFND                          GROUP
     C                   Z-ADD     SCFYR         ARFYR                          FUND YEAR
     C                   Z-ADD     SCCASE        ARCASE                         CASE
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
     C                   Z-ADD     *ZEROS        AREOB#
     C                   Z-ADD     *ZEROS        ARRCN
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
     C*
     C     C1KY          CHAIN     PC101                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   ADD       1             C1LIN#
     C                   UPDATE    PC101
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     C1LIN#        ARINV#                         INVOICE #
     C                   MOVE      *BLANKS       ARSTAT                         STATUS
     C                   MOVEL     SCTCDS        ARDESC                         DESCRIPTION
     C                   Z-ADD     SCCAMT        ARAMT                          DISB CHK AMT
     C                   MOVEL     SCSYS         ARSYS
     C*
     C                   WRITE     PAR01
     C*
     C                   CLEAR                   PIM01
     C*
     C                   Z-ADD     ARCO#         IMCO#
     C                   Z-ADD     ARSUB#        IMSUB#
     C                   Z-ADD     ARFND         IMFND
     C                   Z-ADD     ARFYR         IMFYR
     C                   Z-ADD     ARCASE        IMCASE
     C                   Z-ADD     ARINV#        IMINV#
     C                   Z-ADD     ARTRDT        IMTRDT
     C*
     C                   MOVEL(P)  'A'           IMSTAT
     C                   MOVEL(P)  ARUSER        IMUSER
     C                   MOVEL(P)  ARUSER        IMIUSR
     C                   MOVEL(P)  ARPGM         IMPGM
     C                   MOVEL(P)  ARPGM         IMIPGM
     C*
     C                   Z-ADD     ARTIME        IMTIME
     C                   Z-ADD     ARTIME        IMITIM
     C                   Z-ADD     ARDT          IMDT
     C                   Z-ADD     ARDT          IMIDT
     C*
     C                   WRITE     PIM01
     C*
     C                   Z-ADD     *ZEROS        SCSUB#
     C                   Z-ADD     *ZEROS        SCFND
     C                   Z-ADD     *ZEROS        SCFYR
     C                   Z-ADD     *ZEROS        SCCASE
     C                   MOVEL     *BLANKS       SCSYS
     C                   MOVEL     *BLANKS       SCSYSD
     C                   MOVEL     *BLANKS       SCTCDS
     C                   Z-ADD     *ZEROS        SCCAMT
     C  N16              MOVEL     *BLANKS       SCSS#
     C  N16              Z-ADD     *ZEROS        SCACDT
     C                   Z-ADD     DSDATE        SCPODT
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
