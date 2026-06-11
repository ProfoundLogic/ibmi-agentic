     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0280R01 - MT Outcome/Savings
      *****************************************************************
      *                                                               *
      *  CALLED BY: F0280R - WW Outcome/Savings                       *
      *                                                               *
      *  CALLS TO:                                                    *
      *                                                               *
      *  CREATED:                                                     *
      *                                                               *
      *     05/04/98 C:61 PROJECT 98MAC                               *
      *****************************************************************
      ** REVISIONS:                                                   *
      *                                                               *
      *  12/31/02 C:62 PROJECT MC254                                  *
      *                Use group search/edits if there are group      *
      *                records.                                       *
      *                                                               *
      *  01/26/01 C:62 PROJECT MCMNT    BASE ONLY                     *
      *                Compile only                                   *
      *                                                               *
      *  04/21/99 C:31  Project: 98MBR                                *
      *                 Outcome/Savings: Opt. 20                      *
      *                 2. Add option 20=Change Status                *
      *                                                               *
      *  02/23/99 C:62 PROJECT MC098 16780                            *
      *                GET THE CORRECT SEQ#                           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *  72       Can not be blank                                    *
      *  73       Invalid Table Value                                 *
      *  74       Duplicate record not allowed                        *
      *  75       Must be 'Y' or 'N'                                  *
      *  76 - 77  Position Cursor                                     *
      *  78       Screen updated since entry                          *
      *                                                               *
      *  16/N16   FAMSP RRN/UMPCP RRN
      *  17/N17   THERE ARE GROUP OUTCOME AND SAVINGS RECORDS
      *  60       GENERAL READ/CHAIN                                  *
      *****************************************************************
      *
      *
      *
     FF0280F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Physical file definition (for maintaining record)
     FFDOSP     UF A E             DISK
     F                                     RENAME(POS01:DBRCDF)
      *
      *  Outcome/Savings
     FFDOSL     IF   E           K DISK
     F*** BEGIN ADD *** 02/23/99 **************************************
     FFDOSL3    IF   E           K DISK
     F                                     RENAME(POS01:POS02)
     F*** END   ADD *** 02/23/99 **************************************
      *
      *  Medical Services
     FFAMSP     IF   E             DISK
     F*** BEGIN DEL *** 02/23/99 **************************************
     F**********FAMSL4  IF  E           K        DISK
     F**********  PMS01                             KRENAMEPMS02
     F*** END   DEL *** 02/23/99 **************************************
      *
      *  Pre Cert Master
     FUMPCP     IF   E             DISK
     F*** BEGIN DEL *** 02/23/99 **************************************
     F**********UMPCL12 IF  E           K        DISK
     F**********  PPC01                             KRENAMEPPC02
     F*** END   DEL *** 02/23/99 **************************************
      *
      *  Outcome/Savings File
     FFTOTL     IF   E           K DISK
     FFTOTP     IF   E             DISK
     F                                     RENAME(POT01:POT02)
      *
      *  Generic Table File
     FFTGTL     IF   E           K DISK
      *
      *  Medical Service Code Table File
     FFTMCL     IF   E           K DISK
     F*** BEGIN ADD *** 12/31/02 **************************************
     F*
     F*  Group Outcome/Savings File
     FFTGOL     IF   E           K DISK
     FFTGOP     IF   E             DISK
     F                                     RENAME(PGO01:PGO02)
     FFMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
      /COPY SCOPYMT01D
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*** END   ADD *** 12/31/02 **************************************
      *
      *
     D***PY SCOPYMT01I
      *
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D OPTNM           C                   CONST('Outcome/Savings Inf-
     D                                     ormation')
     D COMAPI          C                   CONST('FA007R')
     D*** BEGIN ADD *** 02/23/99 **************************************
     D CHGNUM          C                   CONST('SA020R')
     D*** END   ADD *** 02/23/99 **************************************
     D #OFSCR          C                   CONST(1)
     D HDMS            C                   CONST('Case Mgmt Svc:')
     D HDPC            C                   CONST('Pre-Cert#:')
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FDOSP
     C                   PARM                    @@RRN1            9 0          FAMSP
     C                   PARM                    @@RRN2            9 0          UMPCP
     C*** BEGIN ADD *** 02/23/99 **************************************
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C*** END   ADD *** 02/23/99 **************************************
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *================================================================
      *  KLIST definitions
      *================================================================
      *
      * Outcome/Savings Table File Key
      *
     C     OTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCODE
      *
      * Outcome/Savings File Key
      *
     C     OSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKMSCD
     C*** BEGIN ADD *** 02/23/99 **************************************
     C                   KFLD                    KKSEQ#
     C*** END   ADD *** 02/23/99 **************************************
     C                   KFLD                    KKDATE
     C                   KFLD                    KKCODE
     C*** BEGIN ADD *** 02/23/99 **************************************
      *
      * Outcome/Savings File Key
      *
     C     OSKYB         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKDATE
     C                   KFLD                    KKCODE
     C*** END   ADD *** 02/23/99 **************************************
      *
      * Generic Table File Key
      *
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** BEGIN DEL *** 02/23/99 **************************************
      *
      * Medical Services File Key
      *
     C********** MSKY      KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           KKSUB#
     C**********           KFLD           KKFND
     C**********           KFLD           KKFYR
     C**********           KFLD           KKCASE
     C**********           KFLD           KKMSCD
      *
      * Utilization Review Pre-Certification Master File Key
      *
     C********** PCKY      KLIST
     C**********           KFLD           KKCO#
     C**********           KFLD           KKSUB#
     C**********           KFLD           KKFND
     C**********           KFLD           KKFYR
     C**********           KFLD           KKCASE
     C**********           KFLD           KKMSCD
     C*** END   DEL *** 02/23/99 **************************************
      *
      *  Medical Service Code Table File
      *
     C     MCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*** BEGIN ADD *** 12/31/02 **************************************
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C     GOKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKCODE
     C*
     C     GOKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*** END   ADD *** 12/31/02 **************************************
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
      *
      *
     C     *LIKE         DEFINE    OSCO#         KKCO#
     C     *LIKE         DEFINE    OSSUB#        KKSUB#
     C     *LIKE         DEFINE    OSFND         KKFND
     C     *LIKE         DEFINE    OSFYR         KKFYR
     C     *LIKE         DEFINE    OSCASE        KKCASE
     C     *LIKE         DEFINE    OSMSCD        KKMSCD
     C     *LIKE         DEFINE    OSDATE        KKDATE
     C     *LIKE         DEFINE    OSCODE        KKCODE
     C     *LIKE         DEFINE    MSSEQ#        KKSEQ#
     C     *LIKE         DEFINE    PCCERT        KKCERT
     C     *LIKE         DEFINE    OSURSV        KKURSV
      *
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
      *
     C/COPY SCOPYMT01C
     C*
     C*** BEGIN ADD *** 04/21/99 **************************************
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C     OSSTAT        IFEQ      'I'
     C                   MOVE      'A'           OSSTAT
     C                   ELSE
     C                   MOVE      'I'           OSSTAT
     C                   ENDIF                                                  STAT = 'I'
     C                   MOVEL     Q1USER        OSUSER
     C                   MOVEL     Q1PGM         OSPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OSDT
     C                   MOVEL     DSTIME        OSTIME
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  *IN99 = Off
     C                   ENDSR
     C*** END ADD *** 04/21/99 ****************************************
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
      *
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
     C     @@MODE        IFEQ      'A'
      *
      *  Date must be valid
      *
     C*****      *IN90     IFEQ '0'
     C*****                Z-ADDSCDATE    SRDTI
     C*****                CALL 'SRCVTDAT'DATCHK
     C*****      SRERR     IFEQ '1'
     C*****                SETON                     9071
     C*****                ENDIF
     C*****                ENDIF
      *
      *  Outcome/Savings Code must be entered
      *
     C     *IN90         IFEQ      '0'
     C     SCCODE        ANDEQ     *BLANKS
     C                   SETON                                        9072
     C                   ENDIF
      *
      *  Outcome/Savings Code must be valid
      *
     C     *IN90         IFEQ      '0'
     C                   MOVEL(P)  SCCODE        KKCODE
     C*** BEGIN ADD *** 12/31/02 **************************************
     C     *IN17         IFEQ      *OFF
     C*** END   ADD *** 12/31/02 **************************************
     C     OTKY          CHAIN     POT01                              60
     C   60              SETON                                        9073
     C*** BEGIN ADD *** 12/31/02 **************************************
     C                   ELSE
     C     GOKY1         CHAIN     PGO01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9073
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 12/31/02 **************************************
     C                   ENDIF
      *
      *  Duplicate records not allowed
      *
     C     *IN90         IFEQ      '0'
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKDATE
     C                   MOVEL(P)  SCCODE        KKCODE
     C*** BEGIN CHG *** 02/23/99 **************************************
     C********** OSKY      CHAINPOS01                60
     C   16OSKY          CHAIN     POS01                              60
     C  N16OSKYB         CHAIN     POS02                              60
     C*** END   CHG *** 02/23/99 **************************************
     C  N60              SETON                                        9074
     C                   ENDIF                                                  DUPLICATE REC
      *
      *
     C                   ENDIF                                                  ADD MODE
      *
      *  Savings Rate defaults to value from FTOTP if not entered
      *
     C                   MOVEL(P)  SCCODE        KKCODE
     C     OTKY          CHAIN     POT01                              60
      *
     C     *IN90         IFEQ      '0'
     C     *IN60         ANDEQ     '0'
     C     SCRATE        ANDEQ     *ZERO
     C                   Z-ADD     OTSVRT        SCRATE
     C                   ENDIF
      *
      *  Less Percentage defaults to value from FTOTP if not entered
      *
     C     *IN90         IFEQ      '0'
     C     *IN60         ANDEQ     '0'
     C     SCPERC        ANDEQ     *ZERO
     C                   Z-ADD     OTPERC        SCPERC
     C                   ENDIF
      *
      *  Calculated Savings                                            ngs
      *
     C                   EXSR      SRCALC
      *
      *  If Savings Amount not entered then defaults to Calculated Savings
      *
     C     *IN90         IFEQ      '0'
     C     SCSAVE        ANDEQ     *ZERO
     C                   Z-ADD     SCCSAV        SCSAVE
     C                   ENDIF
      *
      *  Negotiated Flag must be 'Y' or 'N'
      *
     C     *IN90         IFEQ      '0'
     C     SCNEGO        ANDNE     'Y'
     C     SCNEGO        ANDNE     'N'
     C                   SETON                                        9075
     C                   ENDIF
      *
      *
      * CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
      *
     C     @@MODE        IFEQ      'C'
     C     *IN90         ANDEQ     '0'
      *
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C  N90OSDT          IFGT      SAVEDT
     C     OSDT          OREQ      SAVEDT
     C     OSTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
      *
     C                   ENDIF                                                  @@MODE = 'C'
      *
      *
     C                   ENDSR
      *
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
      *
      *
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
     C     HFLD          WHENEQ    'SCCODE'
     C*** BEGIN ADD *** 12/31/02 **************************************
     C     *IN17         IFEQ      *OFF
     C*** END   ADD *** 12/31/02 **************************************
     C                   CALL      'F2450R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM      *ZERO         ##RRN             9 0          FTOTP
      *
     C     ##RRN         IFEQ      *ZERO
     C                   SETON                                        76
     C                   ELSE
     C     ##RRN         CHAIN     POT02                              60
     C                   MOVEL(P)  OTCODE        SCCODE
     C                   MOVEL(P)  OTDESC        SCCDDS
     C                   MOVEL(P)  OTRTVL        SCRV
     C                   MOVEL(P)  'RTVL'        KKTBID
     C                   MOVEL(P)  OTRTVL        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCRVDS
     C   60              MOVEL(P)  *BLANKS       SCRVDS
     C                   Z-ADD     OTSVRT        SCRATE
     C                   Z-ADD     OTPERC        SCPERC
     C                   SETON                                        77
     C                   ENDIF
     C*** BEGIN ADD *** 12/31/02 **************************************
     C                   ELSE
     C                   CALL      'F2451R'
     C     *INKC         PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZERO         ##RRN             9 0          FTGOP
     C                   PARM      DBRRN         ##RRN2            9 0          FMFNP
      *
     C     ##RRN         IFEQ      *ZERO
     C                   SETON                                        76
     C                   ELSE
     C     ##RRN         CHAIN     PGO02                              60
     C                   MOVEL(P)  GOCODE        SCCODE
     C                   MOVEL(P)  GOCODE        KKCODE
     C     OTKY          CHAIN     POT01                              99
     C                   MOVEL(P)  OTDESC        SCCDDS
     C                   MOVEL(P)  OTRTVL        SCRV
     C                   MOVEL(P)  'RTVL'        KKTBID
     C                   MOVEL(P)  OTRTVL        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCRVDS
     C   60              MOVEL(P)  *BLANKS       SCRVDS
     C                   Z-ADD     OTSVRT        SCRATE
     C                   Z-ADD     OTPERC        SCPERC
     C                   SETON                                        77
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 12/31/02 **************************************
      * ZZZ
     C                   ENDSL
      *
     C                   SETON                                        90
      *
     C                   ENDSR
      *
      *
      *================================================================
      * KNDO - F14=DARTS.
      *================================================================
     C     KNDO          BEGSR
      *
      * ACCESS DARTS
      *
     C                   SELECT
      *
      *    When called from WW Case Management Services F0276R
      *
     C     @@RRN1        WHENNE    *ZERO
     C                   MOVE      KKCO#         #1CO#
     C                   MOVE      KKSUB#        #1SUB#
     C                   MOVE      KKFND         #1FND
     C                   MOVE      KKFYR         #1FYR
     C                   MOVE      KKCASE        #1CASE
     C                   MOVE      KKMSCD        #1MSCD
     C                   MOVE      KKSEQ#        #1SEQ#
     C                   CALL      'FA709R'
     C                   PARM      *BLANK        #1RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1MSCD            4
     C                   PARM                    #1SEQ#            5
     C                   PARM      *BLANKS       #1KEY            40
      *
      *    When called from WW Pre-Certifications
      *
     C     @@RRN2        WHENNE    *ZERO
     C                   MOVE      KKCO#         #1CO#
     C                   MOVE      KKSUB#        #1SUB#
     C                   MOVE      KKFND         #1FND
     C                   MOVE      KKFYR         #1FYR
     C                   MOVE      KKCASE        #1CASE
     C                   MOVE      KKCERT        #1CERT
     C                   CALL      'FA708R'
     C                   PARM      *BLANK        #1RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1CERT           15
     C                   PARM      *BLANKS       #1KEY            40
      *
     C                   ENDSL
      *
     C                   SETON                                        90
      *
     C                   ENDSR
      *
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
      *  Default date to system date
      *
     C     @@MODE        IFEQ      'A'
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SCDATE
      *
     C                   ELSE
     C                   Z-ADD     OSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
     C                   ENDIF                                                  ADD MODE
      *
     C                   MOVEL(P)  OSCODE        SCCODE
      *
     C                   MOVEL(P)  SCCODE        KKCODE
     C     OTKY          CHAIN     POT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  OTDESC        SCCDDS
      *
     C                   MOVEL(P)  OTRTVL        SCRV
     C                   MOVEL(P)  'RTVL'        KKTBID
     C                   MOVEL(P)  OTRTVL        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCRVDS
     C   60              MOVEL(P)  *BLANKS       SCRVDS
      *
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCCDDS
     C                   MOVEL(P)  *BLANKS       SCRV
     C                   MOVEL(P)  *BLANKS       SCRVDS
     C                   ENDIF
      *
     C                   Z-ADD     OSRATE        SCRATE
      *
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     1             SCNUMB
     C                   ELSE
     C                   Z-ADD     OSNUMB        SCNUMB
     C                   ENDIF                                                  ADD MODE
      *
     C                   Z-ADD     OSPERC        SCPERC
      *
      *  Calculated Savings                                            ngs
      *
     C                   EXSR      SRCALC
      *
     C                   Z-ADD     OSSAVE        SCSAVE
      *
     C     @@MODE        IFEQ      'A'
     C                   MOVEL(P)  'N'           SCNEGO
     C                   ELSE
     C                   MOVEL(P)  OSNEGO        SCNEGO
     C                   ENDIF                                                  ADD MODE
      *
     C                   MOVEL(P)  OSCOM1        SCCOM1
     C                   MOVEL(P)  OSCOM2        SCCOM2
     C                   MOVEL(P)  OSCOM3        SCCOM3
     C                   MOVEL(P)  OSCOM4        SCCOM4
     C                   MOVEL(P)  OSCOM5        SCCOM5
      *
     C                   ENDSR
      *
      *
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C                   Z-ADD     KKCO#         OSCO#
     C                   Z-ADD     KKSUB#        OSSUB#
     C                   Z-ADD     KKFND         OSFND
     C                   Z-ADD     KKFYR         OSFYR
     C                   Z-ADD     KKCASE        OSCASE
     C*** BEGIN CHG *** 02/23/99 **************************************
     C*
     C     *IN16         IFEQ      *ON
     C*
     C                   MOVEL(P)  KKMSCD        OSMSCD
     C                   Z-ADD     KKSEQ#        OSSEQ#
     C                   MOVEL(P)  *BLANKS       OSCERT
     C                   MOVEL(P)  *BLANKS       OSURSV
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       OSMSCD
     C                   Z-ADD     *ZEROS        OSSEQ#
     C                   MOVEL(P)  KKCERT        OSCERT
     C                   MOVEL(P)  KKURSV        OSURSV
     C*
     C                   ENDIF
     C*** END   CHG *** 02/23/99 **************************************
      *
     C                   MOVEL(P)  SCCODE        OSCODE
      *
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OSDATE
      *
     C                   Z-ADD     SCRATE        OSRATE
     C                   Z-ADD     SCNUMB        OSNUMB
     C                   Z-ADD     SCPERC        OSPERC
     C                   Z-ADD     SCCSAV        OSCSAV
     C                   Z-ADD     SCSAVE        OSSAVE
     C                   MOVEL(P)  SCNEGO        OSNEGO
      *
     C                   MOVEL(P)  SCCOM1        OSCOM1
     C                   MOVEL(P)  SCCOM2        OSCOM2
     C                   MOVEL(P)  SCCOM3        OSCOM3
     C                   MOVEL(P)  SCCOM4        OSCOM4
     C                   MOVEL(P)  SCCOM5        OSCOM5
      *
     C                   MOVEL(P)  'A'           OSSTAT
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL(P)  Q1USER        OSUSER
     C                   MOVEL(P)  Q1PGM         OSPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OSDT
     C                   MOVEL     DSTIME        OSTIME
      *
     C     @@MODE        IFEQ      'A'
      *
     C                   MOVEL(P)  'A'           OSSTAT
      *
     C                   MOVEL(P)  Q1USER        OSIUSR
     C                   MOVEL(P)  Q1PGM         OSIPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OSIDT
     C                   MOVEL     DSTIME        OSITIM
      *
     C                   ENDIF                                                  @@MODE = A
      *
      *
     C                   ENDSR
      *
      *
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
      *
      *
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
      *
     C     *INKJ         CASEQ     *ON           KJDO                            F10=Search
     C     *INKN         CASEQ     *ON           KNDO                            F14=DARTS
     C                   ENDCS
      *
     C                   ENDSR
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
      *  SET DISPLAY MODE INDICATORS
      *
     C                   SETOFF                                       094041
     C                   SELECT
      *
     C     @@MODE        WHENEQ    'D'                                          DISPLAY MODE
     C                   SETON                                        09
      *
     C     @@MODE        WHENEQ    'A'                                          ADD MODE
     C                   SETON                                        40
      *
     C     @@MODE        WHENEQ    'C'                                          CHG MODE
     C                   SETON                                        41
      *
     C                   ENDSL
      *
      * Get key information
      *
     C                   SELECT
      *
      *    When called from WW Case Management Services F0276R
      *
     C     @@RRN1        WHENNE    *ZERO
     C                   EXSR      SRMSKY
     C*** BEGIN ADD *** 02/23/99 **************************************
     C                   SETON                                        16
     C*** END   ADD *** 02/23/99 **************************************
      *
      *    When called from WW Pre-Certifications
      *
     C     @@RRN2        WHENNE    *ZERO
     C                   EXSR      SRPCKY
     C*** BEGIN ADD *** 02/23/99 **************************************
     C                   SETOFF                                       16
     C*** END   ADD *** 02/23/99 **************************************
      *
     C                   ENDSL
      *
     C*** BEGIN ADD *** 12/31/02 **************************************
     C*
     C* CHECK TO SEE IF THERE IS ANY GROUP OS CODES
     C*
     C                   SETOFF                                       17
     C     GOKY2         CHAIN     PGO01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        17
     C                   ENDIF
     C*
     C* LET'S GET THE GROUP RRN
     C*
     C     *IN17         IFEQ      *ON
     C     FNKY          CHAIN     PFN01                              99
     C                   ENDIF
     C*** END   ADD *** 12/31/02 **************************************
      *
     C                   ENDSR
      *
      *
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
     C                   SELECT
     C*** BEGIN ADD *** 04/21/99 **************************************
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*** END ADD *** 04/21/99 ****************************************
     C                   ENDSR
      *
      *
      *
      *================================================================
      * SRCALC - CALCULATE SAVINGS
      *================================================================
     C     SRCALC        BEGSR
      *  ZZZ
      *
      *  Calculated Savings
      *
      *   sccsav =  (scrate) * (scnumb) * (1.0 - (scperc / 100))
      *
      *
     C     SCPERC        DIV       100           XXPERC           15 5
     C                   Z-ADD     1             XXTMP2
     C                   SUB       XXPERC        XXTMP2           15 5
     C     SCRATE        MULT      SCNUMB        SCCSAV
     C                   MULT      XXTMP2        SCCSAV
      *
     C                   ENDSR
      *
      *
      *================================================================
      * SRMSKY - Get key and header information from Medical Services
      *================================================================
     C     SRMSKY        BEGSR
      *
      *  Get Header from Medical Services file (FAMSP)
      *
     C     @@RRN1        CHAIN     PMS01                              60
      *
     C                   Z-ADD     MSCO#         KKCO#
     C                   Z-ADD     MSSUB#        KKSUB#
     C                   Z-ADD     MSFND         KKFND
     C                   Z-ADD     MSFYR         KKFYR
     C                   Z-ADD     MSCASE        KKCASE
     C                   MOVEL(P)  MSMSCD        KKMSCD
     C                   Z-ADD     MSSEQ#        KKSEQ#
      *
     C*** BEGIN DEL *** 02/23/99 **************************************
     C********** PCKY      CHAINPPC02                60
     C**N60*****           MOVELPCCERT    KKCERT    P
     C**N60*****           MOVELPCURSC    KKURSV    P
     C***60*****           MOVEL*BLANKS   KKCERT    P
     C***60*****           MOVEL*BLANKS   KKURSV    P
     C*** END   DEL *** 02/23/99 **************************************
      *
     C                   MOVE      MSCO#         #1CO#             3
     C                   MOVE      MSSUB#        #1SUB#            5
     C                   MOVE      MSFND         #1FND             3
     C                   MOVE      MSFYR         #1FYR             3
     C                   MOVE      MSCASE        #1CASE            7
      *
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
      *  Create 3rd line of Header
      *
     C                   MOVEL(P)  HDMS          SCL3HD
     C                   CAT(P)    MSMSCD:1      SCL3HD
     C*** BEGIN ADD *** 02/23/99 **************************************
     C                   MOVEL(P)  MSSEQ#        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL     ##NUM         XXSEQ#            5
     C*
     C                   CAT(P)    XXSEQ#:1      SCL3HD
     C*** END   ADD *** 02/23/99 **************************************
     C                   MOVEL(P)  MSMSCD        KKKEY
     C     MCKY          CHAIN     PMC01                              60
     C  N60              CAT(P)    MCDESC:2      SCL3HD
      *
      *
     C                   ENDSR
      *
      *
      *================================================================
      * SRPCKY - Get header information from Pre-cert
      *================================================================
     C     SRPCKY        BEGSR
      *
     C     @@RRN2        CHAIN     PPC01                              60
      *
     C                   Z-ADD     PCCO#         KKCO#
     C                   Z-ADD     PCSUB#        KKSUB#
     C                   Z-ADD     PCFND         KKFND
     C                   Z-ADD     PCFYR         KKFYR
     C                   Z-ADD     PCCASE        KKCASE
     C                   MOVEL(P)  PCMSCD        KKMSCD
      *
     C                   MOVEL(P)  PCCERT        KKCERT
     C                   MOVEL(P)  PCURSC        KKURSV
      *
     C*** BEGIN DEL *** 02/23/99 **************************************
     C********** MSKY      CHAINPMS02                60
     C**N60*****           Z-ADDMSSEQ#    KKSEQ#
     C***60*****           Z-ADD*ZERO     KKSEQ#
     C*** END   DEL *** 02/23/99 **************************************
      *
      *    Get Header Information
      *
     C                   MOVE      PCCO#         #1CO#             3
     C                   MOVE      PCSUB#        #1SUB#            5
     C                   MOVE      PCFND         #1FND             3
     C                   MOVE      PCFYR         #1FYR             3
     C                   MOVE      PCCASE        #1CASE            7
      *
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
      *  Create 3rd line of Header
      *
     C                   MOVEL(P)  HDPC          SCL3HD
     C                   CAT(P)    PCCERT:1      SCL3HD
     C                   CAT(P)    'Ext.':2      SCL3HD
     C                   MOVE      PCEXT         XXTMP1            3
     C                   CAT(P)    XXTMP1:1      SCL3HD
      *
      *
     C                   ENDSR
      *
      *
      *
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FDOSP
     C                   ENDSR
