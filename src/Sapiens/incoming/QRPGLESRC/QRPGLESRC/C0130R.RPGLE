     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H****************************************************************
     H/TITLE C0130R - ADDITIONAL INSURED MAINTENANCE
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                     **
     H**     This program maintains the Additional Interest File    **
     H**     (PAI01).                                               **
     H**   CALLED FROM:                                             **
     H**     ______ - ___________________________                   **
     H**   CALLS TO:                                                **
     H**     C0630R - Casuality Additional Interest Search          **
     H**     C2196R - Additional Interest Search                    **
     H**     M0006C - Help Routine                                  **
     H****************************************************************
     H*
     H****************************************************************
     H** REVISIONS:                                                 **
     H**  11/18/96 *** C:107 *** V2000 ***                          **
     H**           1. Version 2 format changes.                     **
     H**                                                            **
     H****************************************************************
     H*
     H****************************************************************
     H** INDICATOR USAGE:                                           **
     H**   INDICATOR            FUNCTION DESCRIPTION                **
     H**      03       Policy Cancelled                             **
     H**      09       Type 1 call                                  **
     H**      10       Screen help key                              **
     H**      11       Screen valid function keys                   **
     H**      20       Screen 1/2 - CCR0501 Field can not be blank  **
     H**      21       Screen 2 - CCR0501 Field can not be blank    **
     H**      22       Screen 2 - CCR0501 Field can not be blank    **
     H**      23       Screen 2 - CCR0505 State code invalid        **
     H**      24       Screen 2 - CCR0504 Field can not be zero     **
     H**      25       Screen 2 - CCR0590 Invalid Add. Int. Code    **
     H**      26       Screen 2 - CCR0513 Value must be 'Y' or 'N'  **
     H**      27       Screen 2 - CCR0513 Value must be 'Y' or 'N'  **
     H**      40       Additional interest record not found         **
     H**      60       General usage                                **
     H****************************************************************
     H*
     H****************************************************************
     H** FILE USAGE:                                                **
     H**   FILES:                                                   **
     H**   SMCOL  (PCO01): System Company Master File (CO#)         **
     H**   CMUPL  (PUP01): User Parameter File (USR)                **
     H**   CMPML  (PPM01): Policy Master File (CO#,POL#)            **
     H**   CMAIP  (PAI01): Additional Interest (CO#/POL#/IN/ADI)    **
     H**   CMAXP  (PAX01): Additional Insured (NAME)                **
     H****************************************************************
     H*
     FC0130F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FCMPML     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMAIL     UF A E           K DISK
     FCMAXL     IF   E           K DISK
     F*

     D*===============================================================*
     D*     EXTENSION SECTION                                         *
     D*===============================================================*
     D*
     D* DEFINE ARRAYS
     D*
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D ST              S              2    DIM(52) CTDATA PERRCD(26)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D XXAEP           C                   CONST('AS PER VEHICLE SCHED-
     D                                     ULE ATTACHED')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  ADDITIONAL INTEREST FILE
     C*
     C     AIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C                   KFLD                    XXADI             2
     C*
     C*  ADDITIONAL INSURED FILE
     C*
     C     AXKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXNAME
     C     *LIKE         DEFINE    AXNAME        XXNAME
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC             1
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
     C*
     C*
     C*  POLICY MASTER FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCPOL#
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      QQPOL#        SCPOL#
     C     PMKEY         CHAIN     PPM01                              60
     C*
     C*   SET INDICATOR TO DISPLAY POLICY CANCELLED IF CANCELATION
     C*   FLAG(PMCANC) IS EQUAL TO "C"
     C*
     C     PMCANC        IFEQ      'C'
     C                   MOVE      '1'           *IN03
     C                   ELSE
     C                   MOVE      '0'           *IN03
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
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL FUNCTION KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  FUNCTION 01 = END OF JOB
     C*
     C     *INKA         IFEQ      '1'
     C                   MOVE      'A'           QQRTC
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 02 = PRIOR SCREEN
     C*
     C     *INKB         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      'B'           QQRTC
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  FUNCTION 05 = SEARCH LIST OF ADDITIONAL INSURED
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      SCPOL#        QQPOL#           13
     C                   MOVE      SCADI         QQADI             2
     C                   MOVE      ' '           QQTYPE            1
     C                   CALL      'C0630R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQADI
     C                   PARM                    QQTYPE
     C                   PARM                    QQRTC             1
     C                   MOVE      QQADI         SCADI
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     SCADI         OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      *BLANKS       QQNAME           60
     C                   MOVE      *BLANKS       QQRTC
     C                   CALL      'C2196R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQNAME
     C                   PARM                    QQRTC
     C     QQRTC         IFEQ      'B'
     C     QQNAME        OREQ      *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C     QQRTC         IFEQ      '*'
     C     QQNAME        ANDNE     *BLANK
     C                   WRITE     SCR99
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   MOVEL     QQNAME        XXNAME
     C     AXKEY         CHAIN     PAX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      AXNAME        SCNAME
     C                   MOVE      AXADR1        SCADR1
     C                   MOVE      AXADR2        SCADR2
     C                   MOVE      AXCTY         SCCTY
     C                   MOVE      AXST          SCST
     C                   MOVEL     AXZIP         SCZIP1
     C                   MOVE      AXZIP         SCZIP2
     C                   MOVE      AXIN          SCIN
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*   CHANGE STATUS
     C*
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  THE ADDITIONAL INSURED CODE CANNOT BE BLANK
     C*
     C     SCADI         IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCADI         XXADI
     C     AIKEY         CHAIN     PAI01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PAI01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      AINAME        SCNAME
     C                   MOVE      AIADR1        SCADR1
     C                   MOVE      AIADR2        SCADR2
     C                   MOVE      AICTY         SCCTY
     C                   MOVE      AIST          SCST
     C                   MOVEL     AIZIP         SCZIP1
     C                   MOVE      AIZIP         SCZIP2
     C                   MOVE      AIIN          SCIN
     C                   MOVE      AINOR         SCNOR
     C                   MOVE      AINOD         SCNOD
     C                   MOVE      AICOMM        SCCOMM
     C                   MOVE      AIAEP         SCAEP
     C                   MOVE      AISTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SCNAME
     C                   MOVE      *BLANK        SCADR1
     C                   MOVE      *BLANK        SCADR2
     C                   MOVE      *BLANK        SCCTY
     C                   MOVE      *BLANK        SCST
     C                   MOVE      *BLANK        SCZIP1
     C                   MOVE      *BLANK        SCZIP2
     C                   MOVE      *BLANK        SCIN
     C                   MOVE      *BLANK        SCNOR
     C                   MOVE      *BLANK        SCNOD
     C                   MOVE      *BLANK        SCCOMM
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C* BEGIN EDIT CHECKING
     C*
     C*   NAME CANNOT BE BLANK
     C*
     C     SCNAME        CABEQ     *BLANK        STRT                     20
     C*
     C*   ADDRESS CANNOT BE BLANK
     C*
     C     SCADR1        CABEQ     *BLANK        STRT                     21
     C*
     C*   CITY CANNOT BE BLANK
     C*
     C     SCCTY         CABEQ     *BLANK        STRT                     22
     C*
     C*   STATE CANNOT BE BLANK
     C*
     C     SCST          LOOKUP    ST                                     60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   ZIP CANNOT BE ZERO
     C*
     C     SCZIP1        CABEQ     *ZERO         STRT                     24
     C*
     C*   TYPE MUST BE C,L,M,A,O
     C*
     C     SCIN          IFNE      'C'
     C     SCIN          ANDNE     'L'
     C     SCIN          ANDNE     'M'
     C     SCIN          ANDNE     'A'
     C     SCIN          ANDNE     'O'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*         APPLIES TO ENTIRE POLICY
     C*         -------------------------------------------------------
     C     SCAEP         IFNE      'Y'
     C     SCAEP         ANDNE     'N'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C*   NOTICE REQUIRED
     C*
     C     SCNOR         IFNE      'Y'
     C     SCNOR         ANDNE     'N'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     *IN09         IFEQ      '0'
     C                   MOVE      SCADI         XXADI
     C     AIKEY         CHAIN     PAI01                              40
     C                   MOVE      UPCO#         AICO#
     C                   MOVE      SCPOL#        AIPOL#
     C                   MOVE      SCADI         AIADI
     C                   MOVE      SCNAME        AINAME
     C                   MOVE      SCADR1        AIADR1
     C                   MOVE      SCADR2        AIADR2
     C                   MOVE      SCCTY         AICTY
     C                   MOVE      SCST          AIST
     C                   MOVEL     SCZIP1        AIZIP
     C                   MOVE      SCZIP2        AIZIP
     C                   MOVE      SCIN          AIIN
     C                   MOVE      SCNOR         AINOR
     C                   MOVE      SCNOD         AINOD
     C                   MOVE      SCCOMM        AICOMM
     C                   MOVE      SCAEP         AIAEP
     C     SCAEP         IFEQ      'Y'
     C                   MOVE      *BLANKS       AICOMM
     C                   MOVEL     XXAEP         AICOMM
     C                   END
     C                   MOVE      SCSTAT        AISTAT
     C                   MOVE      Q1USER        AIUSER
     C                   MOVE      Q1PGM         AIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AIDT
     C                   Z-ADD     DSTIME        AITIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PAI01
     C                   ELSE
     C                   WRITE     PAI01
     C                   END
     C*
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCADI
     C                   GOTO      STRT
     C*
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPAI01     E            PAI01E
**  TABLE UA
-- ADD --
- UPDATE -
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
