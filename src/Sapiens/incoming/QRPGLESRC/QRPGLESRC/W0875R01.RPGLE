     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W0875R01 - WORK WITH EDI HISTORY LOG UPDATE/DISPLAY
     H*****************************************************************
     H* PROGRAM: W0875R01    C:77      PROJECT: 87MDU
     H* DESCRIPTION: WORK WITH EDI LOG HISTORY MAINTENANCE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/14/22 C:079  Project: WC2797                               *
     H*                 Add IAIABC 3.1.4 field(s)                     *
     H*                                                               *
     H* 06/09/20 C:318  Project: SY420
     H*                 Mask SSN
     H*
     H* 05/30/18 C:079 Project: WC2515                                *
     H*                EDI Release 3.1 Changes(Aquisition Code)       *
     H*                                                               *
     H* 04/05/10 C:189 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*                  PROJECT RE-JAD , RE-DESIGNED                 *
     H*                                                               *
     H* 02/01/10 C:235 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*                                                               *
     H* 12/04/09 C:235 Project: 139WLM PRISS#19                       *
     H*                Creating DART remark for the Log record status *
     H*                change for all 148 log records.                *
     H*
     H* 10/09/09 C:235 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*                                                               *
     H*                                                               *
     H* 07/14/09 C:189 Project :(WC1589 68891)                        *
     H*                xxE1 should work for 148 and xxE2 should work  *
     H*                for A49 ONLY                                   *
     H*                                                               *
     H* 06/04/09 C:189 Project :(WC1589 68684)                        *
     H*                Show and validate MTC's against XXE1,XXE2 first*
     H*                                                               *
     H* 05/29/09 C:189 Project: 139WLM                                *
     H*                 -EDI-Create diary when EDI Log History        *
     H*                                                               *
     H* 10/19/07 C:191 Project :WC1584                                *
     H*                EDI Release 3.0 changes                        *
     H*                                                               *
     H* 05/01/07 C:185 Project:WC1177                                 *
     H*                EDI Release 3 PROCESSING                       *
     H*
     H* 03/01/06 C:167 Project: 48WWM
     H*                Check Mailing Date
     H*
     H* 02/16/06 C:53  SUPPORT: WC132/51005
     H*                IF LOG RCD IS CHANGED FROM "SN" BLANK OUT FIELDS
     H*                THAT ALLOW THE RECORD TO BE RE-SENT.
     H*
     H* 04/27/00 C:53  SUPPORT: WC065/19768
     H*                SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*                                                               *
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**     N40      837/835 EOMB                                   **
     H**      40      278 Treatment plan                             **
     H*****************************************************************
     H*
     FW0875F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FWMLGP     UF A E             DISK
     F                                     RENAME(PLG01:DBRCDF)
     FWTGTL     IF   E           K DISK
     FWTGIL     IF   E           K DISK
     FSTSTL3    IF   E           K DISK
     F*
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCMP     IF   E             DISK
     FWDPTL     UF   E           K DISK
     FWEREL     UF   E           K DISK
     FWDAJL     UF   E           K DISK
     FWMLGL4    IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     F                                     RENAME(PCT01:PCT03)
     FWMIXL     IF   E           K DISK
     FSMUPL1    IF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D*
     D                 DS
     D  SS#                    1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('EDI Log History')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN1            9 0
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*** KEY LIST FOR GROUP AUX FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*   KEY LIST FOR GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKY
     C*
     C     GIKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C*
     C*   KEY LIST FOR STATE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C*   Company Master File
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*   Fund Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*   Claim Master File
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     P0KY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGDSQH
     C*
     C     AJKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGFND
     C                   KFLD                    LGFYR
     C                   KFLD                    LGCASE
     C                   KFLD                    LGDSQM
     C*
     C     LGKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    SCTSET
     C                   KFLD                    SCMTC
     C*
     C     IXKY          KLIST
     C                   KFLD                    LGCO#
     C                   KFLD                    LGJST
     C                   KFLD                    LGTSET
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    LGCO#         KKCO#
     C     *LIKE         DEFINE    LGFND         KKFND
     C     *LIKE         DEFINE    LGFYR         KKFYR
     C     *LIKE         DEFINE    LGCASE        KKCASE
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKY
     C     *LIKE         DEFINE    STKEY         KKKEY
     C     *LIKE         DEFINE    LGSTS         SVSTS
     C     *LIKE         DEFINE    LGSTS         SVSTS#
     C     *LIKE         DEFINE    LGMTC         SVMTC#
     C*
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - Process Hold/Release of log record
     C*================================================================
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C*
     C     LGSTAT        IFEQ      ' '
     C                   MOVE      'H'           LGSTAT
     C                   ELSE
     C                   MOVE      ' '           LGSTAT
     C                   ENDIF
     C*
     C                   MOVEL     Q1USER        LGUSER
     C                   MOVEL     Q1PGM         LGPGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C                   MOVEL     DSTIME        LGTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*================================================================
     C* DATCVT - During use of F10=Search, converts dates to 8.
     C*================================================================
     C     DATCVT        BEGSR
     C*
     C                   Z-ADD     SCDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDUE
     C*
     C                   Z-ADD     SCARCV        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCARCV
     C*
     C*
     C                   Z-ADD     SCMTCD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMTCD
     C*
     C                   Z-ADD     SCMTC2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMTC2
     C                   ENDSR
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C*** Delete records dependent on this log record.
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C     P0KY          CHAIN     PP001                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PP001
     C     P0KY          READE     PP001                                  99
     C                   ENDDO
     C*
     C     P0KY          CHAIN     PRE01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PRE01
     C     P0KY          READE     PRE01                                  99
     C                   ENDDO
     C*
     C     AJKY          CHAIN     PAJ01                              99
     C     *IN99         DOWEQ     *OFF
     C                   DELETE    PAJ01
     C     AJKY          READE     PAJ01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for screen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*** BEG ADD ****** 10/19/07 ************************************
     C*
     C*  Validate the Transaction Set
     C                   MOVEL(P)  'TSET'        KKTBID
     C                   MOVEL(P)  SCTSET        KKKY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  GTDESC        SCTDSC
     C                   ELSE
     C                   SETON                                        9077
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  *IN99 = '0'
     C*
     C*  Validate the MTC Code based on the TSET
     C*
     C*
     C                   SELECT
     C     SCTSET        WHENEQ    '148'
     C                   MOVEL(P)  '  E1'        KKTBID
     C                   MOVEL     SCJST         KKTBID
     C     GIKY          CHAIN     PGI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'FMTC'        KKTBID
     C                   ENDIF
     C     SCTSET        WHENEQ    'A49'
     C                   MOVEL(P)  '  E2'        KKTBID
     C                   MOVEL     SCJST         KKTBID
     C     GIKY          CHAIN     PGI01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'SMTC'        KKTBID
     C                   ENDIF
     C                   ENDSL
     C*
     C                   MOVEL(P)  SCMTC         KKKY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  GTDESC        SCMDSC
     C                   ELSE
     C                   SETON                                        9078
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  *IN99 = '0'
     C*
     C*** CHECK FOR DUPLICATES IF IN ADD MODE
     C*
     C     @@MODE        IFEQ      'A'
     C                   EXSR      CHKDUP
     C                   ENDIF
     C*
     C*
     C*  Validate Check mailed date
     C*
     C     SCMTCD        IFNE      *ZEROS
     C                   Z-ADD     SCMTCD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMTCD
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9086
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  SRERR = '1'
     C*
     C                   ENDIF                                                  SCMTCD <>
     C*
     C*  Require EDI status
     C*
     C                   MOVE      *BLANKS       SCEDSC
     C     SCSTS         IFEQ      *BLANKS
     C                   SETON                                        9071
     C     *IN90         CABEQ     '1'           ERRSTP
     C*
     C                   ELSE
     C*
     C*  Validate the EDI status
     C                   MOVEL(P)  'EDIS'        KKTBID
     C                   MOVEL(P)  SCSTS         KKKY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  GTDESC        SCEDSC
     C                   ELSE
     C                   SETON                                        9071
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  *IN99 = '0'
     C                   ENDIF
     C*
     C*  REQUIRE SUBMISSION DUE DATE
     C*
     C     SCDUE         IFEQ      *ZEROS
     C                   SETON                                        9072
     C     *IN90         CABEQ     '1'           ERRSTP
     C*
     C                   ELSE
     C*
     C*  VALIDATE SUBMISSION DUE DATE
     C*
     C                   Z-ADD     SCDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDUE
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9073
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  SCDUE = *ZEROS
     C*
     C*  VALIDATE DATE RECEIVED
     C*
     C     SCARCV        IFNE      *ZEROS
     C                   Z-ADD     SCARCV        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCARCV
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9075
     C     *IN90         CABEQ     '1'           ERRSTP
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C** CHECK FOR HOLD/RELEASE STATUS
     C*
     C     SCSTAT        IFNE      'H'
     C     SCSTAT        ANDNE     ' '
     C                   SETON                                        9074
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*
     C*
     C     SCMTCC        IFNE      *BLANKS
     C*
     C*  Validate the MTCC Code based on the TSET
     C*
     C                   MOVEL(P)  SCMTCC        KKKY
     C                   MOVEL(P)  'FMTC'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
      *
     C                   ELSE
     C                   MOVEL(P)  'SMTC'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C                   ENDIF
     C*
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  GTDESC        SCCDSC
     C                   ELSE
     C                   SETON                                        9083
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF                                                  *IN99 = '0'
     C                   ENDIF
     C*
     C*  Validate Correction code date
     C*
     C     SCMTC2        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCMTC2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCMTC2
     C*
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9084
     C     *IN90         CABEQ     '1'           ERRSTP
     C*
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  SCMTCD <> '1'
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
     C* KJDO - Process F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** EDI TRANSATION SET ID SEARCH
     C*
     C     HFLD          IFEQ      'SCTSET'
     C                   MOVEL(P)  SCTSET        ##KY
     C*
     C                   CALL      'W2493R'
     C                   PARM      OPTNM         ##PRDS           40
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      'TSET'        ##ID              4
     C                   PARM                    ##KY             10
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C                   SELECT
     C     ##RTCD        WHENEQ    'A'
     C                   MOVE      '1'           *INKC
     C*
     C     ##RTCD        WHENEQ    'B'
     C                   MOVE      '1'           *INKL
     C*
     C     ##RTCD        WHENEQ    '*'
     C                   MOVEL(P)  ##KY          SCTSET
     C                   MOVEL(P)  SCTSET        KKKY
     C                   MOVEL(P)  'TSET'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCTDSC
     C                   SETON                                        79
     C                   ENDSL
     C                   ENDIF
     C*
     C*** EDI MTC CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCMTC'
     C*
     C*** 148 CODE SEARCH
     C*
     C     SCTSET        IFEQ      '148'
     C                   MOVEL(P)  SCMTC         ##KY
     C*
     C                   MOVEL(P)  '  E1'        KKTBID
     C                   MOVEL     SCJST         KKTBID
     C     GIKY          CHAIN     PGI01                              99
     C  N99              MOVEL     KKTBID        ##ID
     C*
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL     'FMTC'        ##ID
     C                   ENDIF
     C*
     C                   CALL      'W2493R'
     C                   PARM      OPTNM         ##PRDS           40
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM                    ##ID              4
     C                   PARM                    ##KY             10
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C                   SELECT
     C     ##RTCD        WHENEQ    'A'
     C                   MOVE      '1'           *INKC
     C*
     C     ##RTCD        WHENEQ    'B'
     C                   MOVE      '1'           *INKL
     C*
     C     ##RTCD        WHENEQ    '*'
     C                   MOVEL(P)  ##KY          SCMTC
     C                   MOVEL(P)  SCMTC         KKKY
     C                   MOVEL(P)  'FMTC'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCMDSC
     C                   SETON                                        80
     C*
     C                   OTHER
     C                   SETON                                        79
     C                   ENDSL
     C                   ENDIF
     C*
     C*** A49 CODE SEARCH
     C*
     C     SCTSET        IFEQ      'A49'
     C                   MOVEL(P)  SCMTC         ##KY
     C*
     C                   MOVEL(P)  '  E2'        KKTBID
     C                   MOVEL     SCJST         KKTBID
     C     GIKY          CHAIN     PGI01                              99
     C  N99              MOVEL     KKTBID        ##ID
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL     'SMTC'        ##ID
     C                   ENDIF
     C*
     C                   CALL      'W2493R'
     C                   PARM      OPTNM         ##PRDS           40
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM                    ##ID              4
     C                   PARM                    ##KY             10
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C                   SELECT
     C     ##RTCD        WHENEQ    'A'
     C                   MOVE      '1'           *INKC
     C*
     C     ##RTCD        WHENEQ    'B'
     C                   MOVE      '1'           *INKL
     C*
     C     ##RTCD        WHENEQ    '*'
     C                   MOVEL(P)  ##KY          SCMTC
     C                   MOVEL(P)  SCMTC         KKKY
     C                   MOVEL(P)  'SMTC'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCMDSC
     C                   SETON                                        80
     C*
     C                   OTHER
     C                   SETON                                        79
     C                   ENDSL
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*** EDI STATUS CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCSTS'
     C*
     C                   MOVEL(P)  SCSTS         ##KY
     C*
     C                   CALL      'W2493R'
     C                   PARM      OPTNM         ##PRDS           40
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      'EDIS'        ##ID              4
     C                   PARM                    ##KY             10
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C                   SELECT
     C     ##RTCD        WHENEQ    'A'
     C                   MOVE      '1'           *INKC
     C*
     C     ##RTCD        WHENEQ    'B'
     C                   MOVE      '1'           *INKL
     C*
     C     ##RTCD        WHENEQ    '*'
     C                   MOVEL(P)  ##KY          SCSTS
     C                   MOVEL(P)  SCSTS         KKKY
     C                   MOVEL(P)  'EDIS'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCEDSC
     C                   SETON                                        76
     C*
     C                   OTHER
     C                   SETON                                        81
     C*
     C                   ENDSL
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C*** EDI MTC CORRECTION CODE SEARCH
     C*
     C     HFLD          IFEQ      'SCMTCC'
     C*
     C                   CALL      'SA711C'
     C                   PARM      *BLANKS       ##RTN             1
     C                   PARM      'W0875R'      ##FNM            10
     C                   PARM      'W0875Q'      ##TNM            10
     C*
     C     ##RTN         IFNE      'A'
     C                   CALL      'W0875Q'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##MODE            1
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    CMFYR
     C                   PARM                    CMCASE
     C                   PARM                    ##RRN             9 0
     C*
      *
     C     ##RRN         CHAIN     DBRCDF                             60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN85
     C                   ELSE
     C                   MOVE      LGMTC         SCMTCC
     C                   Z-ADD     LGMTCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMTC2
      *
     C                   MOVEL(P)  SCMTCC        KKKY
     C                   MOVEL(P)  'FMTC'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCDSC
      *
     C                   ELSE
     C                   MOVEL(P)  'SMTC'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCCDSC
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* KNDO - Process F14=DARTS
     C*================================================================
     C     KNDO          BEGSR
     C*
     C*** ESTABLISH KEY
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      LGCO#         QCCO#
     C                   MOVE      LGFND         QCFND
     C                   MOVE      LGFYR         QCFYR
     C                   MOVE      LGCASE        QCCASE
     C*
     C*** CALL COMMENT PROGRAM
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *BLANKS       SCJST
     C                   MOVE      *BLANKS       SCJNME
     C                   MOVE      *BLANKS       SCTSET
     C                   MOVE      *BLANKS       SCTDSC
     C                   MOVE      *BLANKS       SCMTC
     C                   MOVE      *BLANKS       SCMDSC
     C                   MOVE      *BLANKS       SCSTS
     C                   MOVE      *BLANKS       SCEDSC
     C                   MOVE      *BLANKS       SCSTAT
      /Free
                         scAqSc = *Blanks;
                         scAqScd = *Blanks;
      /End-Free
     C                   Z-ADD     0             SCDUE
     C                   Z-ADD     0             SCEXDT
     C                   Z-ADD     0             SCSENT
     C                   Z-ADD     0             SCARCV
     C*
     C                   MOVE      *BLANKS       SCMTCC
     C                   MOVE      *BLANKS       SCCDSC
     C                   Z-ADD     0             SCMTC2
     C*
     C     LGJST         IFEQ      *BLANKS
     C                   MOVEL(P)  CMJRST        SCJST
     C                   MOVEL(P)  CMJRST        KKKEY
     C                   ELSE
     C                   MOVEL(P)  LGJST         SCJST
     C                   MOVEL(P)  LGJST         KKKEY
     C                   ENDIF
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  STNAME        SCJNME
     C                   ENDIF
     C*
     C                   MOVEL     LGTSET        SCTSET
     C*
     C* GET MAINT TYPE CODE DESCRIPTION
     C*
     C                   SELECT
     C     LGTSET        WHENEQ    '148'
     C                   MOVEL     'FMTC'        KKTBID
     C     LGTSET        WHENEQ    'A49'
     C                   MOVEL     'SMTC'        KKTBID
     C     LGMTC         IFEQ      'AP'
     C     LGMTC         OREQ      'IP'
     C     LGMTC         OREQ      'PY'
     C                   MOVE      '1'           *IN41
     C                   ENDIF
     C                   ENDSL
     C*
     C                   MOVEL(P)  LGMTC         SCMTC
     C*
     C                   SELECT
     C     SCTSET        WHENEQ    '148'
     C                   MOVEL(P)  'FMTC'        KKTBID
     C     SCTSET        WHENEQ    'A49'
     C                   MOVEL(P)  'SMTC'        KKTBID
     C                   ENDSL
     C*
     C                   MOVEL(P)  SCMTC         KKKY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  GTDESC        SCMDSC
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       SCMDSC
     C                   ENDIF
     C                   Z-ADD     LGMTCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMTCD
     C                   MOVEL(P)  LGMTCC        SCMTCC
      *
     C                   MOVEL(P)  SCMTCC        KKKY
     C                   MOVEL(P)  'FMTC'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCDSC
      *
     C                   ELSE
     C                   MOVEL(P)  'SMTC'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCCDSC
     C                   ENDIF
     C                   Z-ADD     LGMTC2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMTC2
     C*
     C                   MOVE      *BLANKS       SCMDSC
     C                   MOVEL(P)  LGMTC         KKKY
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCMDSC
     C*
     C                   MOVEL(P)  LGSTS         SCSTS
     C                   MOVEL(P)  LGSTS         SVSTS
     C*
     C                   MOVEL(P)  LGSTS         KKKY
     C                   MOVEL(P)  'EDIS'        KKTBID
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCEDSC
     C*
     C                   Z-ADD     LGDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDUE
     C*
     C     @@MODE        IFEQ      'A'
     C                   TIME                    DSDTTM
     C*
     C     SCDUE         IFEQ      *ZEROS
     C                   Z-ADD     DSDATE        SCDUE
     C                   ENDIF
     C     SCMTCD        IFEQ      *ZEROS
     C                   Z-ADD     DSDATE        SCMTCD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  LGMTCC        KKKY
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCCDSC
     C*
     C*
     C                   Z-ADD     LGEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C                   Z-ADD     LGSENT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSENT
     C*
     C                   Z-ADD     LGARCV        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCARCV
     C*
     C                   MOVEL(P)  LGSTAT        SCSTAT
      /Free
                         scAqSc = LGaqSc;
                         Select;
                         When scAqSc = 'A';
                            scAqScd = 'Final/Periodic Due';
                         When scAqSc = 'B';
                            scAqScd = 'SROI Due';
                         When scAqSc = 'C';
                            scAqScd = 'No Prior Indemnity Reported';
                         When scAqSc = 'D';
                            scAqScd = 'No Prior Medical Reported';
                         Endsl;
      /End-Free
       //*** Beg Add *** 02/14/22 *********************************************
       scBcRc = LGBcRc;
       //*** End Add *** 02/14/22 *********************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL(P)  SCJST         LGJST
     C                   Z-ADD     KKCO#         LGCO#
     C                   Z-ADD     KKFND         LGFND
     C                   Z-ADD     KKFYR         LGFYR
     C                   Z-ADD     KKCASE        LGCASE
     C                   MOVEL(P)  SCMTC         LGMTC
     C                   MOVEL(P)  SCTSET        LGTSET
     C                   MOVEL(P)  SCMTCC        LGMTCC
     C                   Z-ADD     SCMTC2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGMTC2
     C*
     C                   MOVEL     SCSTS         LGSTS
     C                   MOVEL     SCSTAT        LGSTAT
     C*
     C                   Z-ADD     SCMTCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGMTCD
     C*
     C                   Z-ADD     SCDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDUE
     C*
     C                   Z-ADD     SCARCV        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGARCV
     C*
     C*
     C*  IF STATUS CHANGED FROM "SN" CLEAR DATE SENT
     C*
     C     SVSTS         IFEQ      'SN'
     C     SCSTS         ANDNE     'SN'
     C     SCSTS         ANDNE     'TA'
     C     SCSTS         ANDNE     'TE'
     C     SCSTS         ANDNE     'TR'
     C                   Z-ADD     *ZEROS        LGSENT
     C                   ENDIF
       //*** Beg Add *** 02/14/22 *********************************************
       LGBcRc = scBcRc;
       //*** End Add *** 02/14/22 *********************************************
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
     C                   EXSR      DATCVT
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
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C     @@RRN1        CHAIN     PCM01                              98        Claim info
     C                   MOVE      *OFF          *IN41
     C*
     C                   Z-ADD     CMCO#         KKCO#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C                   MOVEL     LGSTS         SVSTS#
     C                   MOVEL     LGMTC         SVMTC#
     C*
     C*  Load the screen headings.
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   MOVEL(P)  *BLANKS       SCCONM
     C     COKY          CHAIN     PCO01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        SCCONM
     C                   ENDIF
     C*
     C                   Z-ADD     KKFND         SCFND                          Group info
     C     FNKY          CHAIN     PFN01                              99
     C  N99              MOVEL(P)  FNNAME        SCFNAM
     C*
     C*
     C     *IN98         IFEQ      '0'
     C*
     C                   MOVE      CMSS#         SS#                            SS#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'SS#'         QQNAME           25
     C     SS#           PARM      SS#           QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C     SS1           CAT(P)    '-':0         SCSS#
     C                   CAT       SS2:0         SCSS#
     C                   CAT       '-':0         SCSS#
     C                   CAT       SS3:0         SCSS#
     C*
     C     CMLSN         CAT(P)    ',':0         SCCNAM                         Claimant name
     C                   CAT       CMFSN:1       SCCNAM
     C                   CAT       CMMID:1       SCCNAM
     C*
     C                   MOVEL(P)  CMFYR         SCCS#                          Case #
     C                   CAT       '-':0         SCCS#
     C                   MOVE      CMCASE        SCCS#
     C*
     C                   MOVEL(P)  CMACL#        SCSCL#                         State claim #
     C*
     C                   Z-ADD     CMACDT        SRDTI                          Accident date
     C                   CALL      'SRCVTDAT'    DTMDYY                          YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCACDT
     C*
     C                   ENDIF
       //*** Beg Add *** 02/14/22 *********************************************
       Chain(n) (CMco#:CMjrSt) pix01;
       If %Found(WMIXL) and IXrls = 'A';
          isRls31 = *on;
       Else;
          isRls31 = *off;
       Endif;
       //*** End Add *** 02/14/22 *********************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C*
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C*
     C     LGSTS         IFNE      SVSTS#
     C*
     C                   EXSR      DARTS
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1USER        LGUSER
     C                   MOVEL(P)  Q1PGM         LGPGM
     C                   MOVEL(P)  DSTIME        LGTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   EXSR      DARTS
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1USER        LGIUSR
     C                   MOVEL(P)  Q1PGM         LGIPGM
     C                   MOVEL(P)  DSTIME        LGITM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGIDT
     C*
     C                   MOVEL(P)  Q1USER        LGUSER
     C                   MOVEL(P)  Q1PGM         LGPGM
     C                   MOVEL(P)  DSTIME        LGTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LGDT
     C*
     C*  Call api to retrieve the next available log seq#.
     C*
     C                   MOVE      KKCO#         #2CO#
     C                   MOVE      KKFND         #2FND
     C                   MOVE      KKFYR         ##FYR
     C                   MOVE      KKCASE        ##CASE
     C                   MOVE      *ZEROS        ##SEQ#
     C                   CALL      'WA045R'
     C                   PARM                    #2CO#             3
     C                   PARM                    #2FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM                    ##SEQ#            5
     C                   PARM                    ##RTCD
     C*
     C                   MOVE      ##SEQ#        LGDSQH
     C                   MOVE      *OFF          *IN41
     C*
     C                   WRITE     DBRCDF
     C*
     C                   MOVE      '1'           OUTFLG
     C*
     C                   ENDSR
     C*================================================================
     C* CHKDUP - Check for a duplicate log record/status
     C*================================================================
     C     CHKDUP        BEGSR
     C*
     C     LGKY          CHAIN     PLG01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     LGSTS         IFEQ      'EI'                                         ACTIVE LOG RECORD
     C     LGSTS         OREQ      'EC'
     C                   SETON                                        8290
     C                   GOTO      ENDDUP
     C                   ENDIF
     C*
     C     LGKY          READE     PLG01                                  60
     C                   ENDDO
     C*
     C     ENDDUP        ENDSR
     C     DARTS         BEGSR
     C*
     C     LGTSET        IFEQ      '148'
     C     LGTSET        OREQ      'A49'
     C*
     C     IXKY          CHAIN     PIX01                              60
     C*
     C                   SELECT
     C     LGSTS         WHENEQ    'RE'
     C     IXCRER        ANDNE     *BLANKS
     C                   MOVE      IXCRER        Q#RMCD            5
     C*
     C     LGSTS         WHENEQ    'EI'
     C     IXCEIR        ANDNE     *BLANKS
     C                   MOVE      IXCEIR        Q#RMCD            5
     C*
     C     LGSTS         WHENEQ    'EC'
     C     IXCECR        ANDNE     *BLANKS
     C                   MOVE      IXCECR        Q#RMCD            5
     C*
     C     LGSTS         WHENEQ    'TA'
     C     IXCTAR        ANDNE     *BLANKS
     C                   MOVE      IXCTAR        Q#RMCD
     C*
     C     LGSTS         WHENEQ    'TR'
     C     IXCTRR        ANDNE     *BLANKS
     C                   MOVE      IXCTRR        Q#RMCD
     C*
     C                   OTHER
     C                   GOTO      END
     C                   ENDSL
     C*
     C                   MOVE      LGCO#         QCCO#
     C                   MOVE      LGFND         QCFND
     C                   MOVE      LGFYR         QCFYR
     C                   MOVE      LGCASE        QCCASE
     C                   MOVEL(P)  QCKEY         Q#KEY
     C*
     C     Q#RMCD        CHAIN     PCT03                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
     C                   MOVEL(P)  CTDESC        Q#COMM
     C                   MOVEL(P)  CTFL01        Q#DIFL            1
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C*
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C*
     C     CMCLEX        CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   UPUSR         Q#USER           10
     C                   ENDIF
     C     Q#USER        IFEQ      *BLANKS
     C                   GOTO      END
     C                   ENDIF
     C     LGSTS         IFEQ      'EC'
     C     IXCECR        ANDNE     *BLANKS
     C                   MOVE(P)   Q1USER        Q#USER           10
     C                   ENDIF
     C     LGSTS         IFEQ      'TA'
     C     IXCTAR        ANDNE     *BLANKS
     C                   MOVE(P)   Q1USER        Q#USER           10
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C*
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  Q#KEY         Q#FOLD           80
     C                   ELSE
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   ENDIF
     C*
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C*
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  'Y'           Q#FL01            1
     C                   ELSE
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   ENDIF
     C*
     C     CTDPRG        IFGT      0
     C                   MOVEL(P)  'Y'           Q#FL02            1
     C                   ELSE
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   ENDIF
     C*
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   MOVEL     *BLANKS       Q#COMM
     C     LGMTC         CAT       '-':0         Q#COMM
     C                   CAT       CTDESC:0      Q#COMM
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY            40
     C                   PARM                    Q#COMM          256
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C                   ENDIF
     C     END           ENDSR
