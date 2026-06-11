     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F2510R01 - EDI 148/A49 EDIT MATRIX MAINTENANCE
     H*
     H*****************************************************************
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  This program will allow the user to maintain the EDI 149/A49 *
     H*  Edit Matrix Table                                            *
     H*                                                               *
     H*   SCR01 edits                                                 *
     H*   - The Message Number must be greater than 0                 *
     H*   - The Message Number must be valid in Error Message Table   *
     H*   - The Description is required                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/10/12 C:15  PROJECT: SY266D                               *
     H*                 Remove second subfile from screen.            *
     H*                                                               *
     H*  04/11/02 C:62  PROJECT: MCMNT LOG# 217
     H*                 WHEN CALLED TO CHANGE STATUS, MAKE SURE IT
     H*                 CHANGES THE STATUS AND RETURNS TO CALLING
     H*                 PROGRAM.
     H*
     H*  04/11/02 C:62  PROJECT: MCMNT LOG# 216
     H*                 USE SY600R INSTEAD OF F0601R.
     H*
     H*  04/27/00 C:53  SUPPORT: WC065/19768
     H*                 SWAP FILE STABL FOR STSTL3 TO OMIT "UC" ENTRY.
     H*
     H*****************************************************************
     H*  CALLED BY: F2510R                                            *
     H*  CALLS TO:                                                    *
     H*             F2500R (TABLE SEARCH FOR ERROR MESSAGES)          *
     H*             F2505R (TABLE SEARCH FOR CRITERIA)                *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60 - GENERAL CHAIN INDICATOR                                 *
     H*                                                               *
     H*  SCR01:                                                       *
     H*                                                               *
     H*  30 - MESSAGE NUMBER MUST BE > 0                              *
     H*  22 - MESSAGE NUMBER INVALID IN ERROR MESSAGE TABLE           *
     H*                                                               *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*  F2510F01 WORKSTATION FILE                                    *
     H*  SMCOL:  (PCO01) CO#                                          *
     H*  SMUPL:  (PUP01) USR                                          *
     H*  FTMXL:  (PMX01) CO#/JST/DN#/SET/MTC/MSG#                     *
     H*  FTERL:  (PER01) CO#/MSG#                                     *
     H*  FTERP:  (PER02) CO#/MSG#                                     *
     H*  FTDNL:  (PDN01) RRN                                          *
     H*  FTDNP:  (PDN02) CO#/JST/DN#                                  *
     H*                                                               *
     H*****************************************************************
     FF2510F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN1)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFTERL     IF   E           K DISK
     FFTERP     IF   E             DISK
     F                                     RENAME(PER01:PER02)
     FFTDNL     IF   E           K DISK
     FFTDNP     IF   E             DISK
     F                                     RENAME(PDN01:PDN02)
     FFTMXL     UF A E           K DISK
     F*** BEGIN CHG *** 04/11/02 **************************************
     F**********FTMXP   IF  E                    DISK
     FFTMXP     UF   E             DISK
     F*** END   CHG *** 04/11/02 **************************************
     F                                     RENAME(PMX01:DBRCDF)
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*** BEGIN DEL *** 04/11/02 **************************************
     F**********STSTP   IF  E                    DISK
     F*** END   DEL *** 04/11/02 **************************************
     F*** BEG CHG *** 04/27/00 **********************************
     F**STABL***IF**E********** K        DISK
     FSTSTL3    IF   E           K DISK
     F*** END CHG *** 04/27/00 **********************************
     F                                     RENAME(PST01:PST02)
     D*
     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXCSR                  1     10
     D  XXPOS                  6      6  0
     D*================================================================
     D*  Constants
     D*================================================================
     D OPTNM           C                   CONST('EDI Edit Matrix-
     D                                      Information')
     D*================================================================
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C* GENERIC TABLE FILE PARM LIST
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C*   USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE ERROR MESSAGE TABLE
     C*
     C     ERKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKMSG#
     C*
     C*  KEY LIST FOR THE EDIT MATRIX TABLE
     C*
     C     MXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKJST
     C                   KFLD                    KKDN#
     C*
     C     MXKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKJST
     C                   KFLD                    KKDN#
     C                   KFLD                    KKTSET
     C                   KFLD                    KKMTC
     C*
     C     MXKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKJST
     C                   KFLD                    KKDN#
     C                   KFLD                    KKTSET
     C                   KFLD                    KKMTC
     C                   KFLD                    KKMSG#
     C*
     C*  KEY LIST FOR Data element criteria
     C*
     C     DNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKJST
     C                   KFLD                    KKTSET
     C                   KFLD                    KKDN#
     C*
     C*   ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*** BEGIN ADD *** 04/11/02 **************************************
     C*
     C* CHANGE STATUS
     C*
     C     @@MODE        IFEQ      'S'
     C*
     C                   EXSR      CHGSTS
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C*** END   ADD *** 04/11/02 **************************************
     C*
     C*   PLIST to call Error search program - W2269R2
     C*
     C     ERSPL         PLIST
     C                   PARM      '1'           QQCALL            1
     C                   PARM      KKCO#         QQCO#             3 0
     C                   PARM      SCMSG#        QQMSG#            3 0
     C                   PARM      *BLANKS       QQRTCD            1
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
     C                   MOVE      *ON           *IN71
     C*
     C     *LIKE         DEFINE    UPCO#         KKCO#
     C     *LIKE         DEFINE    MXJST         KKJST
     C     *LIKE         DEFINE    MXDN#         KKDN#
     C     *LIKE         DEFINE    MXTSET        KKTSET
     C     *LIKE         DEFINE    MXMTC         KKMTC
     C     *LIKE         DEFINE    MXMSG#        KKMSG#
     C     *LIKE         DEFINE    MXDESC        KKDESC
     C     *LIKE         DEFINE    MXSTAT        KKDLTF
     C     *LIKE         DEFINE    *IN99         LODFLG
     C     *LIKE         DEFINE    *IN99         UPFLG
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    STKEY         KKKEY
     C*
     C                   MOVE      *OFF          LODFLG
     C                   MOVE      *OFF          UPFLG
     C*
     C*   GET THE USER PARAMETER FILE
     C*          =============
     C     TOP           TAG
     C*          =============
     C     UPKY          CHAIN     PUP01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      *ZERO         UPCO#
     C*
     C                   ENDIF
     C*
     C                   MOVE      UPCO#         KKCO#
     C*
     C*   GET THE COMPANY RECORD
     C*
     C                   MOVE      *BLANKS       SCCONM
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL     OPTNM         SCPRDS
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCJST
     C                   Z-ADD     *ZEROS        SCDN#
     C                   MOVE      *BLANK        SCTSET
     C                   MOVE      *BLANK        SCMTC
     C                   MOVE      *BLANK        SCMODE
     C                   MOVE      '0'           KKDLTF
     C*
     C*   GET TARGET RECORD
     C*
     C*** BEGIN CHG *** 04/11/02 **************************************
     C********** @@RRN     CHAINDBRCDF               99
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*** END   CHG *** 04/11/02 **************************************
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      MXJST         KKJST
     C                   MOVE      MXDN#         KKDN#
     C                   MOVE      MXTSET        KKTSET
     C                   MOVE      MXMTC         KKMTC
     C                   MOVE      MXMSG#        KKMSG#
     C*
     C                   MOVE      MXJST         SCJST
     C                   MOVE      MXDN#         SCDN#
     C                   MOVE      MXTSET        SCTSET
     C*
     C                   MOVEL(P)  SCJST         KKJST
     C                   MOVEL(P)  SCTSET        KKTSET
     C                   Z-ADD     SCDN#         KKDN#
     C*
     C     DNKY          CHAIN     PDN01                              99
     C*
     C   99              MOVEL     *BLANKS       SCDESC
     C  N99              MOVEL     DNDESC        SCDESC
     C*
     C                   MOVE      MXMTC         SCMTC
     C*
     C                   ENDIF
     C*
     C     TOPL          TAG
     C*
     C                   EXSR      LOAD
     C*
     C*   DISPLAY ALL FORMATS
     C*          =============
     C     STRT          TAG
     C*          =============
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C     Q2RECF        OREQ      'SCR01S'
     C     Q2RECF        OREQ      'SCR01C'
     C*
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      *OFF          *IN90
     C*
     C                   ENDIF
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*   F3 = END OF JOB
     C*
     C     *INKC         IFEQ      *ON
     C*
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C*
     C*   F10 = Search for message
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C     SFRRN         CHAIN     SCR01S                             43
     C*
     C     *IN43         IFEQ      *OFF
     C*
     C                   EXSR      KJDOS
     C*
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  IN43=OFF
     C                   ENDIF                                                  INKJ=ON
     C*
     C*   F12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C*
     C                   ENDIF
     C*
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C*
     C                   CALL      'F0006R'
     C******             WRITE     SCR99
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      *ZERO         UPCO#
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C*
     C                   ENDIF
     C                   ENDIF                                                     IN11=*on
     C*
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR01S'      SCR01T
     C     Q2RECF        CABEQ     'SCR01C'      SCR01T
     C*
     C*
     C*   SCREEN 1 EDITS
     C*          =============
     C     SCR01T        TAG
     C*          =============
     C     *IN09         CABEQ     '1'           NOEDT
     C*
     C*    Validate top fields if ADD
     C*
     C     *IN40         IFEQ      *ON
     C*
     C   43*INKJ         CASEQ     *ON           KJDOC
     C                   ENDCS
     C*
     C                   MOVE      *ON           UPFLG
     C*
     C* VALIDATE THE JURISDICTION STATE
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCJST         KKKEY
     C*
     C     STKY          CHAIN     PST02                              99
     C*
     C   99              SETON                                        7290
     C*
     C                   ENDIF
     C*
     C* VALIDATE THE TRANSACTION SET ID
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCTSET        KKTKEY
     C                   MOVEL(P)  'TSET'        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7390
     C*
     C                   ENDIF
     C*
     C* VALIDATE MAINTENANCE TYPE CODE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMTC         ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  SCMTC         KKTKEY
     C                   MOVEL(P)  'MTC '        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7690
     C*
     C                   ENDIF
     C*
     C* VALIDATE DATA ELEMENT NUMBER
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   MOVEL(P)  SCJST         KKJST
     C                   MOVEL(P)  SCTSET        KKTSET
     C                   Z-ADD     SCDN#         KKDN#
     C                   MOVEL(P)  SCMTC         KKMTC
     C*
     C     DNKY          CHAIN     PDN01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   SETON                                        7790
     C*
     C                   ELSE                                                   X-*IN99
     C*
     C                   MOVEL     DNDESC        SCDESC
     C*
     C                   ENDIF                                                  *IN99 = ON
     C                   ENDIF                                                  *IN90 = OFF
     C*
     C                   ENDIF                                                  *IN40 = ON
     C*
     C* VALIDATE MAINTENANCE TYPE CODE
     C*
     C     *IN90         IFEQ      *OFF
     C     SCMTC         ANDNE     *BLANKS
     C*
     C                   MOVEL(P)  SCMTC         KKTKEY
     C                   MOVEL(P)  'MTC '        KKTBID
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C   99              SETON                                        7890
     C*
     C                   ENDIF
     C*
     C     *IN90         CABEQ     *ON           STRT
     C*
     C*               PROCESS SUBFILE CHANGES
     C*   VALIDATE MESSAGE NUMBER AGAINST ERROR MESSAGE TABLE
     C*   ENSURE DESCRIPTION IS NOT BLANK IF MSG#>0
     C*
     C                   Z-ADD     1             RRN1              5 0
     C*
     C                   READC     SCR01S                                 50
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C                   MOVE      *ON           UPFLG
     C                   MOVE      *ON           LODFLG
     C*
     C*     DELETE REQUEST
     C*
     C     SFSEL         IFEQ      '4'
     C                   Z-ADD     0             SFMSG#
     C                   MOVE      *BLANKS       SFSEL
     C                   MOVE      *BLANKS       SFDESC
     C                   UPDATE    SCR01S
     C                   ENDIF
     C*
     C     SFMSG#        IFEQ      *ZERO
     C*
     C                   MOVE      *BLANKS       SFDESC
     C                   MOVE      *BLANKS       SFSEL
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SFMSG#        KKMSG#
     C*
     C     ERKY          CHAIN     PER01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      *ON           *IN30
     C                   MOVE      *ON           *IN90
     C                   UPDATE    SCR01S
     C*
     C                   ENDIF
     C*
     C     *IN99         IFEQ      *OFF
     C     SFDESC        ANDEQ     *BLANKS
     C*
     C                   MOVE      ERDESC        SFDESC
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN86
     C                   UPDATE    SCR01S
     C                   MOVE      *OFF          *IN86
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   READC     SCR01S                                 50
     C*
     C                   ENDDO
     C*
     C     *IN90         CABEQ     *ON           STRT
     C*
     C*   ALL EDITS PASSED, UPDATE TABLE FILE IF CHANGES
     C*
     C     UPFLG         CABEQ     *OFF          TERM
     C*
     C*   FIRST DELETE ALL DATABASE RECORDS
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C     *IN99         DOUEQ     *ON
     C*
     C     MXKY2         DELETE    PMX01                              99
     C*
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C*   NOW WRITE ALL FROM SUBFILE
     C*
     C                   Z-ADD     1             RRN1
     C*
     C     RRN1          CHAIN     SCR01S                             50
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C     SFMSG#        IFGT      0
     C*
     C                   Z-ADD     SFMSG#        KKMSG#
     C*
     C     MXKY3         CHAIN(N)  PMX01                              99
     C*
     C     *IN99         IFEQ      *ON
     C*
     C                   MOVE      KKCO#         MXCO#
     C                   MOVE      SCJST         MXJST
     C                   MOVE      SCDN#         MXDN#
     C                   MOVE      SCTSET        MXTSET
     C                   MOVE      SCMTC         MXMTC
     C                   MOVE      SFMSG#        MXMSG#
     C                   MOVE      SFDESC        MXDESC
     C                   MOVE      'A'           MXSTAT
     C                   MOVE      Q1USER        MXUSER
     C                   MOVE      Q1USER        MXIUSR
     C                   MOVE      Q1PGM         MXPGM
     C                   MOVE      Q1PGM         MXIPGM
     C                   Z-ADD     DSTIME        MXTIME
     C                   Z-ADD     DSTIME        MXITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MXDT
     C                   MOVEL     SRDTO         MXIDT
     C*
     C                   WRITE     PMX01
     C*
     C                   ENDIF                                                     SFSEL/4
     C*
     C                   ENDIF                                                     SFSEL/4
     C*
     C                   ADD       1             RRN1
     C*
     C     RRN1          CHAIN     SCR01S                             50
     C*
     C                   ENDDO
     C*
     C     LODFLG        CABEQ     *ON           TOPL
     C*
     C                   GOTO      TERM
     C*
     C     NOEDT         TAG
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   Z-ADD     *ZEROS        SCDN#
     C                   MOVE      *BLANKS       SCTSET
     C                   MOVE      *BLANKS       SCMTC
     C*
     C                   ENDIF
     C*
     C                   GOTO      STRT
     C*
     C*   END THE PROGRAM
     C*          =============
     C     TERM          TAG
     C*          =============
     C                   SETON                                        LR
     C*** BEGIN ADD *** 04/11/02 **************************************
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C*
     C     MXSTAT        IFEQ      'I'
     C                   MOVE      'A'           MXSTAT
     C                   ELSE
     C                   MOVE      'I'           MXSTAT
     C                   ENDIF                                                  MXSTAT
     C*
     C                   MOVEL     Q1USER        MXUSER
     C                   MOVEL     Q1PGM         MXPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MXDT
     C*
     C                   MOVEL     DSTIME        MXTIME
     C*
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
     C*** END   ADD *** 04/11/02 **************************************
     C*****************************************************************====*
     C*** *   SEARCH SUBROUTINE (SCR01) - TOP                 K J D O C6 ***
     C*****************************************************************====*
     C     KJDOC         BEGSR
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCJST'
     C*
     C*** BEGIN CHG *** 04/11/02 **************************************
     C**********           CALL 'F0601R'
     C**********           PARM '2'       ##CALL  1
     C********** *INKC     PARM '0'       ##KC    1
     C********** *INKL     PARM '0'       ##KL    1
     C**********           PARM *ZEROS    ##RRN   90
     C**********           PARM *ZEROS    ##RRN2  90
     C*
     C********** *INKL     IFEQ *ON
     C********** ##RRN     OREQ *ZEROS
     C*
     C**********           SETON                     3106
     C*
     C**********           ELSE
     C*
     C********** ##RRN     CHAINPST01                99
     C*
     C**********           MOVELSTKEY     SCJST     P
     C**********           SETON                     3206
     C*
     C**********           ENDIF
     C**********
     C                   CALL      'SY600R'
     C                   PARM      *BLANKS       ##ST              2
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##ST          OREQ      *BLANKS
     C                   SETON                                        3106
     C                   ELSE
     C                   MOVEL     ##ST          SCJST
     C                   SETON                                        3206
     C                   ENDIF
     C*** END   CHG *** 04/11/02 **************************************
     C*
     C     HFLD          WHENEQ    'SCTSET'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'TSET'        ##TBID            4
     C*
     C     ##RRN         IFEQ      *ZEROS
     C*
     C                   SETON                                        3206
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL(P)  GTKEY         SCTSET
     C                   SETON                                        3306
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCMTC'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'MTC '        ##TBID            4
     C*
     C     ##RRN         IFEQ      *ZEROS
     C*
     C                   SETON                                        3306
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL(P)  GTKEY         SCMTC
     C                   SETON                                        3406
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDN#'
     C*
     C                   CALL      'F2500R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      SCJST         ##JST             2
     C                   PARM      SCTSET        ##TSET            3
     C*
     C     ##RRN         IFEQ      *ZEROS
     C*
     C                   SETON                                        3406
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PDN02                              99
     C*
     C                   SETON                                        3506
     C                   MOVEL(P)  DNJST         SCJST
     C                   MOVEL(P)  DNTSET        SCTSET
     C                   Z-ADD     DNDN#         SCDN#
     C                   MOVEL(P)  DNDESC        SCDESC
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*****************************************************************====*
     C*** *   SEARCH SUBROUTINE (SCR01) - SUBFILE             K J D O S6 ***
     C*****************************************************************====*
     C*
     C     KJDOS         BEGSR
     C*
     C                   CALL      'F2505R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
     C     ##RRN         IFEQ      *ZEROS
     C*
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PER02                              99
     C*
     C                   Z-ADD     ERMSG#        SFMSG#
     C                   MOVEL(P)  ERDESC        SFDESC
     C*
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*****************************************************************====*
     C*** *   LOAD SUBROUTINE   (SCR01)                       L O A D  6 ***
     C*****************************************************************====*
     C*
     C     LOAD          BEGSR
     C*
     C                   MOVE      *OFF          LODFLG
     C*
     C     MXKY          SETLL     PMX01
     C*
     C                   MOVE      *OFF          *IN75                          SFLEND * NO *
     C                   MOVE      *OFF          *IN35
     C*
     C*     Clear subfile
     C*
     C                   MOVE      *ON           *IN74                          SFLCLR * YES*
     C                   MOVE      *OFF          *IN71                          SFLDSP * NO *
     C*
     C                   WRITE     SCR01C
     C*
     C                   MOVE      *OFF          *IN74                          SFLCLR * NO *
     C                   MOVE      *ON           *IN71                          SFLDSP * YES*
     C*
     C                   DO        99            RRN1
     C*
     C     MXKY2         READE     PMX01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C     RRN1          ANDEQ     1
     C*
     C                   MOVEL     UA(1)         SCMODE
     C                   MOVE      '1'           *IN35                               END OF FILE
     C*
     C                   ENDIF
     C*
     C                   MOVEL     *BLANK        SFSEL
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     UA(2)         SCMODE
     C                   MOVE      MXMSG#        SFMSG#
     C                   MOVEL     MXDESC        SFDESC
     C                   Z-ADD     RRN1          XXRRN1            5 0
     C                   MOVE      *ON           *IN86
     C*
     C                   ELSE                                                   99 = '0'
     C*
     C                   MOVEL     *BLANK        SFSEL
     C                   Z-ADD     *ZEROS        SFMSG#
     C                   MOVEL     *BLANK        SFDESC
     C                   MOVE      *ON           *IN75                          SFLEND *
     C*
     C                   ENDIF
     C*
     C                   WRITE     SCR01S
     C                   MOVE      *OFF          *IN86
     C*                                                            %%
     C                   ENDDO
     C*
     C*   TO POSITION SCREEN AND CURSOR
     C*
     C                   Z-ADD     1             #@$2
     C*
     C                   ENDSR
**  TABLE UA
-- Add --
- Change -
