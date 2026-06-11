     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H/TITLE N0421R - A/R SUMMARY
     H*****************************************************************
     H* PROGRAM DESCRIPTION: A/R SUMMARY, SUMMARIZES ALL INVOICES     *
     H*                      FOR A PARTICULAR CASE.                   *
     H*                                                               *
     H* CALLED FROM: N0420R - A/R INQUIRY - CASE LEVEL                *
     H* CALLED TO  : NONE                                             *
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
     H*  08/02/02 C:62 PROJECT AR001                                  *
     H*                COMPILE ONLY                                   *
     H*                                                               *
     H*****************************************************************
     FN0421F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMCMP     IF   E             DISK
     FNMARL5    IF   E           K DISK
     FFMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
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
     D QQPRDS          C                   CONST('               A/R S-
     D                                     ummary              ')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMDT          XXFRDT
     C     *LIKE         DEFINE    CMDT          XXTODT
     C     *LIKE         DEFINE    ##L1          XXHD1
     C     *LIKE         DEFINE    ##L2          XXHD2
     C*
     C     ARKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C*   KEY LIST FOR THE COMPANY FILE  (SMCOL)
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  USER PARAMETER FILE  (FMUPL)
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM      *BLANKS       @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0          NMIMP
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
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
     C     @@RRN         IFNE      *ZEROS
     C*
     C     @@RRN         CHAIN     PCM01                              99
     C                   SETON                                        1617
     C                   SETON                                        1819
     C                   Z-ADD     CMCO#         KKCO#
     C*
     C                   Z-ADD     CMSUB#        SCSUB#
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     CMSS#         XXSS#
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
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   ELSE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C                   SETOFF                                       1617
     C                   SETOFF                                       1819
     C                   Z-ADD     UPCO#         KKCO#
     C*
     C                   SELECT
     C*
     C     @@CASE        WHENNE    *ZEROS
     C*
     C                   Z-ADD     @@SUB#        SCSUB#
     C                   Z-ADD     @@FND         SCFND
     C                   Z-ADD     @@FYR         SCFYR
     C                   Z-ADD     @@CASE        SCCASE
     C                   SETON                                        27
     C*
     C     CMKY          CHAIN     PCM02                              99
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     CMSS#         XXSS#
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
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     @@FYR         WHENNE    *ZEROS
     C*
     C                   Z-ADD     @@SUB#        SCSUB#
     C                   Z-ADD     @@FND         SCFND
     C                   Z-ADD     @@FYR         SCFYR
     C  N32              SETON                                        35
     C*
     C     @@FND         WHENNE    *ZEROS
     C*
     C                   Z-ADD     @@SUB#        SCSUB#
     C                   Z-ADD     @@FND         SCFND
     C  N32
     CANN35              SETON                                        34
     C*
     C     @@SUB#        WHENNE    *ZEROS
     C*
     C                   Z-ADD     @@SUB#        SCSUB#
     C  N32
     CANN35
     CANN34              SETON                                        33
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCCONM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SCDATE
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     @@CALL        COMP      '1'                                    09
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SCTODT
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
     C/SPACE 3
     C*
     C**********************************
     C*   F10 = SEARCH                 *
     C**********************************
     C     *INKJ         IFEQ      '1'
     C     *IN19         ANDEQ     *OFF
     C*
     C     HFLD          IFEQ      'SCSUB#'
     C     HFLD          OREQ      'SCFND'
     C     HFLD          OREQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   CALL      'F0269R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      SCSUB#        ##SUB#            5 0
     C                   PARM      SCFND         ##FND             3 0
     C                   PARM      SCFYR         ##FYR             3 0
     C                   PARM      *ZEROS        ##CASE            7 0
     C     XXSS#         PARM      *BLANKS       ##SS#             9
     C                   PARM      *ZEROS        ##ACDT            8 0
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##CASE        IFEQ      *ZEROS
     C*
     C                   GOTO      STRT
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ##SUB#        SCSUB#
     C                   Z-ADD     ##FND         SCFND
     C                   Z-ADD     ##FYR         SCFYR
     C                   Z-ADD     ##CASE        SCCASE
     C                   Z-ADD     ##ACDT        SCACDT
     C*
     C     SCFRDT        IFEQ      *ZEROS
     C*
     C                   Z-ADD     ##ACDT        SCFRDT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   ENDIF
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     XXSS#1        X2SS#1
     C***                MOVEL     '-'           X2HYP1
     C***                MOVEL     XXSS#2        X2SS#2
     C***                MOVEL     '-'           X2HYP2
     C***                MOVEL     XXSS#3        X2SS#3
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'XXSS#':XXSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   SETON                                        32
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C**********************************
     C*   F12 = PREVIOUS               *
     C**********************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   GOTO      TERM
     C*
     C                   ENDIF                                                  ENDIF SCR02
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
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
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C***************************
     C* PROCESS REQUEST SCREEN  *
     C***************************
     C     SCR01T        TAG
     C*
     C* VALIDATE CASE
     C*
     C     CMKY          CHAIN     PCM02                              99
     C*
     C   99              SETON                                        24
     C   99              GOTO      STRT
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     CMSS#         XXSS#
     C***                MOVEL     XXSS#1        X2SS#1
     C***                MOVEL     '-'           X2HYP1
     C***                MOVEL     XXSS#2        X2SS#2
     C***                MOVEL     '-'           X2HYP2
     C***                MOVEL     XXSS#3        X2SS#3
      /Free
       xxSS# = MaskData('SSN':'CMSS#':CMSS#);
       scSS# = DisplaySS#(MaskData('SSN':'CMSS#':XXSS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C*   VALIDATE FROM DATE
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   MOVE      '1'           *IN25                          INVALID DATE
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  ENDIF SRERR
     C*
     C*   VALIDATE TO DATE
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   MOVE      '1'           *IN26                          INVALID DATE
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  ENDIF SRERR
     C*
     C* MAKE SURE FROM DATE IS NOT GREATER THAN TO DATE
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     XXFRDT        IFGT      XXTODT
     C*
     C                   SETON                                        27
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*  ALL EDITS HAVE BEEN PASSED:  WRITE RECORDS TO A/R FILE
     C*
     C* CALL API TO CREATE A TWO LINE HEADING
     C*
     C                   MOVE      CMCO#         #1CO#             3
     C                   MOVE      CMSUB#        #1SUB#            5
     C                   MOVE      CMFND         #1FND             3
     C                   MOVE      CMFYR         #1FYR             3
     C                   MOVE      CMCASE        #1CASE            7
     C*
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C     XXHD1         PARM      *BLANK        ##L1             78
     C     XXHD2         PARM      *BLANK        ##L2             78
     C*
     C                   MOVEL     XXHD1         SCHD1
     C                   MOVEL     XXHD2         SCHD2
     C                   MOVE      XXHD1         SCHD3
     C                   MOVE      XXHD2         SCHD4
     C*
     C                   Z-ADD     *ZEROS        SCIAMT
     C                   Z-ADD     *ZEROS        SCPAMT
     C                   Z-ADD     *ZEROS        SCNAMT
     C                   Z-ADD     *ZEROS        SCVAMT
     C                   Z-ADD     *ZEROS        SCAAMT
     C                   Z-ADD     *ZEROS        SCSAMT
     C                   Z-ADD     *ZEROS        SCPBAL
     C                   Z-ADD     *ZEROS        SCADUE
     C*
     C* LOAD SCR02 FIELDS
     C*
     C     ARKY          CHAIN     PAR01                              99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     ARTRDT        IFLT      XXFRDT
     C*
     C     ARTRTP        IFEQ      'P'
     C*
     C                   SUB       ARAMT         SCPBAL
     C*
     C                   ELSE
     C*
     C                   ADD       ARAMT         SCPBAL
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C     ARTRDT        IFGE      XXFRDT
     C     ARTRDT        ANDLE     XXTODT
     C*
     C                   SELECT
     C*
     C     ARTRTP        WHENEQ    'I'
     C                   ADD       ARAMT         SCIAMT
     C*
     C     ARTRTP        WHENEQ    'P'
     C                   ADD       ARAMT         SCPAMT
     C*
     C     ARTRTP        WHENEQ    'N'
     C                   ADD       ARAMT         SCNAMT
     C*
     C     ARTRTP        WHENEQ    'V'
     C                   ADD       ARAMT         SCVAMT
     C*
     C     ARTRTP        WHENEQ    'A'
     C                   ADD       ARAMT         SCAAMT
     C*
     C     ARTRTP        WHENEQ    'S'
     C                   ADD       ARAMT         SCSAMT
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C     ARKY          READE     PAR01                                  99
     C*
     C                   ENDDO
     C*
     C     SCPBAL        SUB       SCPAMT        SCADUE
     C                   ADD       SCIAMT        SCADUE
     C                   ADD       SCNAMT        SCADUE
     C                   ADD       SCVAMT        SCADUE
     C                   ADD       SCAAMT        SCADUE
     C                   ADD       SCSAMT        SCADUE
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C***********************
     C*  END THE PROGRAM    *
     C***********************
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
