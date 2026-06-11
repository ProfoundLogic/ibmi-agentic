     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H**********************************************************
     H* PROGRAM: F0175R01
     H* DESCRIPTION: WORK WITH CASE STATUS HISTORY
     H**********************************************************
     H* INDICATORS:
     H**********************************************************
     H* REVISIONS:                                             *
     H*                                                        *
     H* 06/09/20 C:212 Project : SY420                         *
     H*                  Mask SSN                              *
     H*                  Recompile Only                        *
     H*                                                        *
     H* 01/26/01 C:62 PROJECT: MCMNT  BASE ONLY                *
     H*               POPULATE STATUS DATE, TIME AND USER      *
     H*                                                        *
     H* 10/14/99 C:53 PROJECT: MCMNT                           *
     H*               CORRECT THE DISPLAY OF CLOSURE CODE IN DISPLAY
     H*               MODE
     H*                                                        *
     H* 06/26/98 C:94 PROJECT: MC145                           *
     H*               CHANGED DISPLAY IN ADD MODE SO THAT      *
     H*               CLOSURE CODE DID NOT DISPLAY IF IT WAS A *
     H*               REOPEN.                                  *
     H*                                                        *
     H* 02/05/98 C:62 PROJECT: MC145                           *
     H*               ** CHANGED FROM DISPLAY TABLE TO CODE    *
     H*                  FROM GENERIC TABLE FILE.              *
     H*                                                        *
     H* 01/27/98 C:62 PROJECT: MC145                           *
     H*               1) ADDED SA001R - ACCESS FLAGS           *
     H*               2) ADDED FA703R - DARTS                  *
     H*                                                        *
     H* 01/08/98 C:86 PROJECT: MC145                           *
     H*               DISPLAY FILE F0175F01 HAS BEEN CHANGED TO*
     H*               TO PRINT A 2 LINE SUB-HEADING. THIS PRO- *
     H*               GRAM CALLS FA702R TO PRINT IT & USES LIKE*
     H*               DEFINES TO REDEFINE THE OMMITTED SCREEN  *
     H*               FIELDS.                                  *
     H*                                                        *
     H* 02/18/97 C:56  87MAZ
     H*                CREATED
     H**********************************************************
     FF0175F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFMCHP     UF A E             DISK
     F                                     RENAME(PCH01:DBRCDF)
     FFMCMP     IF   E             DISK
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D OPTNM           C                   CONST('Case Status History -
     D                                     Information')
     D COMAPI          C                   CONST('FA007R')
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0

     I*

     C*
     C*****************************************************************
     C**                                                             **
     C**  IF CALLED FROM CASE MANAGEMENT, THE 'OPEN/CLOSE' STATUS IS **
     C**  PASSED (@@OCST) AND THE RELATIVE RECORD NUMBER IS ZERO.    **
     C**  IF CALLED FOR THE SELECTION (SEARCH) PROGRAM, THE SUBFILE  **
     C**  RELATIVE RECORD NUMBER (@@RRN) IS PASSED AND THE           **
     C**  'OPEN/CLOSE' STATUS IS BLANK.                              **
     C**                                                             **
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@OCST            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*
     C     DRTSPL        PLIST
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    ##WCIN
     C                   PARM                    ##WCDK
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C                   PARM      *BLANKS       ##KEY            40
     C*
     C     GENTPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID            4
     C*
     C     *LIKE         DEFINE    CHEFDT        HLEFDT
     C************ BEGIN ADD ******************** 01/08/98 ************
     C     *LIKE         DEFINE    CHCO#         KKCO#
     C************ END   ADD ******************** 01/08/98 ************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DATCVT - Pre-processing before delete occurs
     C*================================================================
     C     DATCVT        BEGSR
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   SETOFF                                       30
     C*
     C*** VALIDATE EXPLANATION - CANNOT BE BLANK
     C*
     C     SCEXPL        IFEQ      *BLANKS
     C                   SETON                                        9082
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C*** VALIDATE EFFECTIVE DATE
     C*
     C     SCEFDT        IFEQ      *ZEROS
     C                   SETON                                        9083
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C     SCEFDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9084
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCEFDT IFNE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLEFDT
     C*
     C     HLEFDT        IFLT      CMACDT
     C                   SETON                                        9085
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  HLEFDT IFEQ
     C*
     C*
     C*** VALIDATE JOB STATUS INFORMATION IF OPEN/CLOSE STATUS = C
     C*
     C     @@OCST        IFEQ      'C'
     C                   MOVEL     'CLCD'        KKTBID
     C                   MOVEL     SCCLCD        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C   99              SETON                                        8690
     C                   ENDIF                                                  @@OCST IFEQ
     C*
     C     ERRSTP        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SETOFF                                       30
     C*
     C                   EXSR      DATCVT
     C*
     C* CLOSURE CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCCLCD'
     C*
     C                   MOVEL     'CLCD'        ##TBID
     C*
     C                   CALL      'F2380R'      GENTPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        3090
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SCCLCD
     C                   MOVEL     GTDESC        SCCDSC
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C                   EXSR      DATCVT
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1CO#
     C                   MOVE      CMFND         #1CO#
     C                   MOVE      CMFYR         #1FYR
     C                   MOVE      CMCASE        #1FYR
     C*
     C                   CALL      'FA703R'      DRTSPL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   SETOFF                                         10      9
     C*
     C*** DISPLAY AND INFORMATION SETUP BASED ON MODE AND STATUS
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C     @@OCST        IFEQ      'C'
     C                   SETOFF                                         8081
     C                   ELSE
     C                   SETON                                          8081
     C                   ENDIF                                                   @@MODE IFEQ
     C*** BEG ADD *** 06/26/98 ***************************************
     C     @@OCST        IFEQ      'R'
     C                   SETON                                        1090
     C                   ENDIF
     C*** END ADD *** 06/26/98 ***************************************
     C                   ENDIF                                                   CHOCST IFEQ
     C*
     C     @@MODE        IFEQ      'D'
     C     CHOCST        IFEQ      'C'
     C                   MOVE      *OFF          *IN10                          ADD 10/14/99
     C                   SETOFF                                         80
     C                   ELSE
     C                   SETON                                          80
     C                   MOVE      *ON           *IN10                          ADD 10/14/99
     C                   ENDIF                                                  @@MODE IFEQ
     C                   ENDIF                                                  CHOCST IFEQ
     C*
     C                   MOVE      *BLANKS       SCOCST
     C                   MOVE      *BLANKS       SCEXPL
     C                   Z-ADD     0             SCEFDT
     C                   MOVE      *BLANKS       SCCLCD
     C                   MOVE      *BLANKS       SCCDSC
     C*
     C     @@MODE        IFEQ      'A'
     C                   MOVEL     @@OCST        SCOCST
     C                   ELSE
     C                   MOVEL     CHOCST        SCOCST
     C                   MOVEL     CHEXPL        SCEXPL
     C                   Z-ADD     CHEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVEL     CHCLCD        SCCLCD
     C*
     C                   MOVEL     'CLCD'        KKTBID
     C                   MOVEL     SCCLCD        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL     GTDESC        SCCDSC
     C   99              MOVE      *BLANKS       SCCDSC
     C*
     C                   ENDIF                                                  @@MODE IFEQ
     C*
     C*** BEGIN ADD *** 01/26/01 **************************************
     C     *IN40         IFEQ      *OFF
     C                   Z-ADD     CHIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCIDT
     C                   Z-ADD     CHITIM        SCITIM
     C                   MOVEL(P)  CHIUSR        SCIUSR
     C                   ELSE
     C                   Z-ADD     *ZEROS        SCIDT
     C                   Z-ADD     *ZEROS        SCITIM
     C                   MOVEL(P)  *BLANKS       SCIUSR
     C                   ENDIF
     C*** END   ADD *** 01/26/01 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     CMCO#         CHCO#
     C                   Z-ADD     CMSUB#        CHSUB#
     C                   Z-ADD     CMFND         CHFND
     C                   Z-ADD     CMFYR         CHFYR
     C                   Z-ADD     CMCASE        CHCASE
     C                   MOVEL     SCOCST        CHOCST
     C                   MOVEL     SCEXPL        CHEXPL
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CHEFDT
     C*
     C                   MOVEL     SCCLCD        CHCLCD
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C     *INKN         CASEQ     *ON           KNDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C*
     C*** GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*********** BEGIN CHANGE ******************* 01/08/98 ***********
     C*********            MOVEL@@SBNM    SCSBNM
     C*********            MOVE @@EMP#    SCEMP#
     C*********            MOVE @@DIV     SCDIV
     C*********            MOVEL@@EMNM    SCEMNM
     C*********            MOVEL@@SS#     SCSS#
     C*********            MOVEL@@CLNM    SCCLNM
     C*********            MOVEL@@SCL#    SCSCL#
     C*
     C* GET CLAIM MASTER RECORD.
     C*
     C     @@RRN2        CHAIN     PCM01                              99
     C*
     C                   MOVE      CMCO#         KKCO#
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
     C     SCL1HD        PARM      *BLANK        ##L1             78
     C     SCL2HD        PARM      *BLANK        ##L2             78
      *
     C*********** END   CHANGE ******************* 01/08/98 ***********
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record after processing
     C*================================================================
     C     UPDATR        BEGSR
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   TIME                    DFDTTM
     C                   MOVEL     Q1USER        CHIUSR
     C                   MOVEL     Q1USER        CHUSER
     C                   MOVEL     Q1PGM         CHIPGM
     C                   MOVEL     Q1PGM         CHPGM
     C                   MOVEL     DFTIME        CHITIM
     C                   MOVEL     DFTIME        CHTIME
     C*
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CHIDT
     C                   MOVEL     SRDTO         CHDT
     C*
     C                   WRITE     DBRCDF
     C                   MOVE      '1'           OUTFLG
     C*
     C                   ENDSR
