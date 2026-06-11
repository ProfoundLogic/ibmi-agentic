     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*
     H/TITLE W2069R3 - EDI 148/A49 EDIT MATRIX MAINTENANCE
     H*
     H*****************************************************************
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H*  04/10/12 C:15  PROJECT: SY266D                               *
     H*                 Remove second subfile from screen.            *
     H*                                                               *
     H*  05/23/02  C:53   Project: WC577
     H*                   Correct ability to change status.
     H*                                                               *
     H*  12/01/99  C:118  PROJECT: WC577                              *
     H*                   Made V3 changes to display file.            *
     H*                                                               *
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
     H*  CALLED BY: MENU                                              *
     H*  CALLS TO:
     H*             W2269R2 (TABLE SEARCH FOR ERROR MESSAGES)         *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60 - GENERAL CHAIN INDICATOR                                 *
     H*                                                               *
     H*  SCR01:                                                       *
     H*                                                               *
     H*  21 - MESSAGE NUMBER MUST BE > 0                              *
     H*  22 - MESSAGE NUMBER INVALID IN ERROR MESSAGE TABLE           *
     H*                                                               *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*  W2069F03 WORKSTATION FILE                                    *
     H*  SMCOL:  (PCO01) CO#                                          *
     H*  WMUPL:  (PUP01) USR                                          *
     H*  WTEML:  (PEM01) CO#/JST/DN#/SET/MTC/MSG#                     *
     H*  WTERL:  (PER01) CO#/MSG#                                     *
     H*  WTDCL:  (PDC01) CO#/JST/DN#                                  *
     H*                                                               *
     H*****************************************************************
     FW2069F03  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(SCR01S:RRN1)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTERL     IF   E           K DISK
     FWTDCL     IF   E           K DISK
     FWTEML     UF A E           K DISK
     F*** BEG CHG *** 05/23/02 ****************************
     F*WTEMP***IF  E                    DISK
     FWTEMP     UF   E             DISK
     F*** BEG CHG *** 05/23/02 ****************************
     F                                     RENAME(PEM01:DBRCDF)
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
     C                   KFLD                    XXCO#
     C*
     C*   USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE ERROR MESSAGE TABLE
     C*
     C     ERKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXMSG#
     C*
     C*  KEY LIST FOR THE EDIT MATRIX TABLE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXDN#
     C*
     C*  KEY LIST FOR Data element criteria
     C*
     C     DCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXDN#
     C*
     C     EMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJST
     C                   KFLD                    XXDN#
     C                   KFLD                    XXTSET
     C                   KFLD                    XXMTC
     C*----------------------------------------------------------------
     C*   ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*   PLIST to call Error search program - W2269R2
     C*
     C     ERSPL         PLIST
     C                   PARM      '1'           QQCALL            1
     C                   PARM      XXCO#         QQCO#             3 0
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
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    EMJST         XXJST
     C     *LIKE         DEFINE    EMDN#         XXDN#
     C     *LIKE         DEFINE    EMSET         XXTSET
     C     *LIKE         DEFINE    EMMTC         XXMTC
     C     *LIKE         DEFINE    EMMSG#        XXMSG#
     C     *LIKE         DEFINE    EMDESC        XXDESC
     C     *LIKE         DEFINE    EMSTAT        XXDLTF
     C     *LIKE         DEFINE    *IN60         LODFLG
     C     *LIKE         DEFINE    *IN60         UPFLG
     C                   MOVE      *OFF          LODFLG
     C                   MOVE      *OFF          UPFLG
     C*
     C*   GET THE USER PARAMETER FILE
     C*          =============
     C     TOP           TAG
     C*          =============
     C     UPKY          CHAIN     PUP01                              60
     C   60              MOVE      *ZERO         UPCO#
     C                   MOVE      UPCO#         XXCO#
     C*
     C*   GET THE COMPANY RECORD
     C*
     C                   MOVE      *BLANKS       SCCONM
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
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
     C                   MOVE      '0'           XXDLTF
     C*
     C*   GET TARGET RECORD
     C*** BEG ADD *** 05/23/02 ****************************
     C     @@MODE        IFEQ      'S'
     C     @@RRN         CHAIN     DBRCDF                             99
     C                   ELSE
     C*** END ADD *** 05/23/02 ****************************
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   ENDIF                                                  ADD 05/23/02
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C*** BEG ADD *** 05/23/02 ****************************
     C* STATUS CHANGE
     C     @@MODE        IFEQ      'S'
     C*
     C     EMSTAT        IFEQ      'I'
     C                   MOVE      'A'           EMSTAT
     C                   ELSE
     C                   MOVE      'I'           EMSTAT
     C                   ENDIF                                                  emSTAT
     C*
     C                   MOVEL     Q1USER        EMUSER
     C                   MOVEL     Q1PGM         EMPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C                   MOVEL     DSTIME        EMTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
     C*
     C*** END ADD *** 05/23/02 ****************************
     C*
     C                   MOVE      EMJST         XXJST
     C                   MOVE      EMDN#         XXDN#
     C                   MOVE      EMSET         XXTSET
     C                   MOVE      EMMTC         XXMTC
     C                   MOVE      EMMSG#        XXMSG#
     C*
     C                   MOVE      EMJST         SCJST
     C                   MOVE      EMDN#         SCDN#
     C                   MOVE      EMSET         SCTSET
     C                   MOVE      EMMTC         SCMTC
     C                   ENDIF
     C*
     C     TOPL          TAG
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
     C                   WRITE     SCR01
     C                   EXFMT     SCR01C
     C                   MOVE      *OFF          *IN91
     C                   ENDIF
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      *ON
     C*
     C*   F3 = END OF JOB
     C*
     C     *INKC         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*   F10 = Search for message
     C*
     C     *INKJ         IFEQ      *ON
     C     SFRRN         CHAIN     SCR01S                             43
     C     *IN43         IFEQ      *OFF
     C                   Z-ADD     0             QQMSG#
     C                   CALL      'W2269R2'     ERSPL                  99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     QQMSG#        SFMSG#
     C                   Z-ADD     QQMSG#        XXMSG#
     C     ERKY          CHAIN     PER01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ERDESC        SFDESC
     C                   ENDIF                                                  IN60=OFF
     C                   UPDATE    SCR01S
     C                   GOTO      STRT
     C                   ENDIF                                                  IN99=OFF
     C                   ENDIF                                                  IN43=OFF
     C*
     C                   ENDIF                                                  INKJ=ON
     C*
     C*   F12 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
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
     C     *IN40         IFEQ      *ON
     C*
     C                   MOVE      *ON           UPFLG
     C*
     C     SCJST         IFEQ      *BLANK
     C                   MOVE      *ON           *IN25
     C                   MOVE      *ON           *IN91
     C                   ENDIF
     C*
     C     *IN91         IFEQ      *OFF
     C     SCDN#         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN26
     C                   MOVE      *ON           *IN91
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN91         IFEQ      *OFF
     C                   MOVEL     SCJST         XXJST
     C                   Z-ADD     SCDN#         XXDN#
     C     DCKY          CHAIN     PDC01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN27
     C                   MOVE      *ON           *IN91
     C                   ELSE                                                   X-*IN60
     C                   MOVEL     DCDESC        SCECDS
     C                   ENDIF                                                  *IN60 = ON
     C                   ENDIF                                                  *IN90 = OFF
     C*
     C                   ENDIF                                                  *IN40 = ON
     C     *IN91         CABEQ     *ON           STRT
     C*
     C*               PROCESS SUBFILE CHANGES
     C*   VALIDATE MESSAGE NUMBER AGAINST ERROR MESSAGE TABLE
     C*   ENSURE DESCRIPTION IS NOT BLANK IF MSG#>0
     C*
     C                   Z-ADD     1             RRN1              5 0
     C                   READC     SCR01S                                 50
     C     *IN50         DOWEQ     *OFF
     C*
     C                   MOVE      *ON           UPFLG
     C                   MOVE      *ON           LODFLG
     C*
     C*     DELETE REQUEST
     C     SFSEL         IFEQ      '4'
     C                   Z-ADD     0             SFMSG#
     C                   MOVE      *BLANKS       SFSEL
     C                   MOVE      *BLANKS       SFDESC
     C                   UPDATE    SCR01S
     C                   ENDIF
     C*
     C     SFMSG#        IFEQ      *ZERO
     C                   MOVE      *BLANKS       SFDESC
     C                   MOVE      *BLANKS       SFSEL
     C                   ELSE
     C                   Z-ADD     SFMSG#        XXMSG#
     C     ERKY          CHAIN     PER01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   MOVE      *ON           *IN91
     C                   UPDATE    SCR01S
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C     SFDESC        ANDEQ     *BLANKS
     C                   MOVE      ERDESC        SFDESC
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN86
     C                   UPDATE    SCR01S
     C                   MOVE      *OFF          *IN86
     C                   ENDIF
     C                   ENDIF
     C*
     C                   READC     SCR01S                                 50
     C                   ENDDO
     C*
     C     *IN91         CABEQ     *ON           STRT
     C*
     C*   ALL EDITS PASSED, UPDATE TABLE FILE IF CHANGES
     C     UPFLG         CABEQ     *OFF          TERM
     C*
     C*   FIRST DELETE ALL DATABASE RECORDS
     C     *IN60         DOUEQ     *ON
     C     EMKY2         DELETE    PEM01                              60
     C                   ENDDO
     C*
     C*   NOW WRITE ALL FROM SUBFILE
     C*
     C                   Z-ADD     1             RRN1
     C     RRN1          CHAIN     SCR01S                             50
     C*
     C     *IN50         DOWEQ     *OFF
     C*
     C     SFMSG#        IFGT      0
     C                   MOVE      XXCO#         EMCO#
     C                   MOVE      SCJST         EMJST
     C                   MOVE      SCDN#         EMDN#
     C                   MOVE      SCTSET        EMSET
     C                   MOVE      SCMTC         EMMTC
     C                   MOVE      SFMSG#        EMMSG#
     C                   MOVE      SFDESC        EMDESC
     C                   MOVE      'A'           EMSTAT
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
     C                   Z-ADD     DSTIME        EMTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C*
     C                   WRITE     PEM01
     C                   ENDIF                                                     SFSEL/4
     C*
     C                   ADD       1             RRN1
     C     RRN1          CHAIN     SCR01S                             50
     C                   ENDDO
     C     LODFLG        CABEQ     *ON           TOPL
     C*
     C                   GOTO      TERM
     C*
     C     NOEDT         TAG
     C*
     C                   Z-ADD     *ZEROS        SCDN#
     C                   MOVE      *BLANKS       SCTSET
     C                   MOVE      *BLANKS       SCMTC
     C                   GOTO      STRT
     C*
     C*   END THE PROGRAM
     C*          =============
     C     TERM          TAG
     C*          =============
     C                   MOVE      '1'           *INLR
     C*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++====*
     C*** *   LOAD SUBROUTINE   (SCR01)                       L O A D  6 ***
     C*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++====*
     C*
     C     LOAD          BEGSR
     C                   MOVE      *OFF          LODFLG
     C*
     C     EMKY          SETLL     PEM01
     C*
     C                   MOVE      *OFF          *IN75                          SFLEND * NO *
     C                   MOVE      *OFF          *IN32
     C*
     C*     Clear subfile
     C*
     C                   MOVE      *ON           *IN74                          SFLCLR * YES*
     C                   MOVE      *OFF          *IN71                          SFLDSP * NO *
     C                   WRITE     SCR01C
     C                   MOVE      *OFF          *IN74                          SFLCLR * NO *
     C                   MOVE      *ON           *IN71                          SFLDSP * YES*
     C*
     C                   DO        99            RRN1
     C*
     C     EMKY2         READE     PEM01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C     RRN1          ANDEQ     1
     C                   MOVEL     UA(1)         SCMODE
     C                   MOVE      '1'           *IN32                               END OF FILE
     C                   ENDIF
     C*
     C                   MOVEL     *BLANK        SFSEL
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UA(2)         SCMODE
     C                   MOVE      EMMSG#        SFMSG#
     C                   MOVEL     EMDESC        SFDESC
     C                   Z-ADD     RRN1          XXRRN1            5 0
     C                   MOVE      *ON           *IN86
     C*
     C                   ELSE                                                   60 = '0'
     C*
     C                   MOVEL     *BLANK        SFSEL
     C                   Z-ADD     *ZEROS        SFMSG#
     C                   MOVEL     *BLANK        SFDESC
     C                   MOVE      *ON           *IN75                          SFLEND *
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
