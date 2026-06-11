     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4110R01 - SAMPLE ADDITIONAL INPUT PROGRAM
     H* ***************************************************************
     H* PROGRAM DESCRIPTION                                           *
     H*                                                               *
     H* This program prompts the user for additional input necessary  *
     H* to print a department letter.                                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W4110R                                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*  PROGRAMMER: C:09                                             *
     H*                                                               *
     H*  CREATED:    03/22/95               PROJECT: WC375            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FW4115F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWSDLL     UF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMDEL     IF   E           K DISK
     FWMDOL     IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D*   ADDITIONAL PROGRAM PARMS
     D*
     D                 DS
     D  PARMS                  1    164
     D  QQCO#                  1      3
     D  QQFND                  4      6
     D  QQREQ#                 7     11
     D  QQDPCD                12     16
     D  QQDCID                17     28
     D  QQDESC                29     56
     D  QQFLDR                57    136
     D  QQFEMP               137    145
     D  QQFDIV               146    150
     D  QQTEMP               151    159
     D  QQTDIV               160    164
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  DEPT/DOC ID MASTER FILE
     C*
     C     DOKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    QQDPCD
     C                   KFLD                    QQFLDR
     C                   KFLD                    QQDCID
     C*
     C*  DEPT MASTER FILE
     C*
     C     DEKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    QQDPCD
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*   KEY FOR LETTER WORKFILE
     C*
     C     DLKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXREQ#
     C                   KFLD                    QQDPCD
     C                   KFLD                    QQFLDR
     C                   KFLD                    QQDCID
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    DLFND         XXFND
     C     *LIKE         DEFINE    DLREQ#        XXREQ#
     C     *LIKE         DEFINE    DEDEPT        XXDEPT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQPRMS          164
     C                   PARM                    XXDEPT
     C                   PARM                    QQRTCD            1
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
     C                   MOVE      QQPRMS        PARMS
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQREQ#        XXREQ#
     C                   MOVE      *BLANK        QQRTCD
     C*
     C*  GET THE USER PARAMETER FILE
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
     C*  GET THE FUND RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C*  GET THE DEPT LETTER WORKFILE RECORD
     C*
     C     DLKEY         CHAIN     PDL01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *BLANKS       SC40A1
     C                   MOVE      *BLANKS       SC40A2
     C                   MOVE      *BLANKS       SC40A3
     C                   MOVE      *BLANKS       SC40A4
     C                   MOVE      *BLANKS       SC40A5
     C                   ELSE
     C                   MOVEL     DL40A1        SC40A1
     C                   MOVEL     DL40A2        SC40A2
     C                   MOVEL     DL40A3        SC40A3
     C                   MOVEL     DL40A4        SC40A4
     C                   MOVEL     DL40A5        SC40A5
     C                   END
     C*
     C*  GET DEPT/DOCUMENT RECORD
     C*
     C     DOKEY         CHAIN     PDO01                              60
     C*
     C*  GET DEPT. RECORD
     C*
     C     DEKEY         CHAIN     PDE01                              60
     C*
     C*   ASSIGN SIGN HEADER FIELDS
     C*
     C                   MOVEL     DEDPNM        SCDPNM
     C                   MOVEL     DEDPCD        SCDPCD
     C                   MOVEL     QQDCID        SCDCID
     C                   MOVEL     DODESC        SCDESC
     C                   MOVE      UPFND         SCFND
     C                   MOVE      QQFEMP        SCFEMP
     C                   MOVE      QQFDIV        SCFDIV
     C                   MOVE      QQTEMP        SCTEMP
     C                   MOVE      QQTDIV        SCTDIV
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
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
     C********************************
     C*   PROCESS ALL COMMAND KEYS   *
     C********************************
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*****************
     C*   F3 = EXIT   *
     C*****************
     C*
     C     *INKC         IFEQ      '1'
     C*
     C*          BEFORE RETURNING, DELETE THE RECORD IN WSDLL.
     C*
     C     DLKEY         DELETE    PDL01                              59
     C*
     C                   MOVE      'A'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C********************
     C*   F12 = RETURN   *
     C********************
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*          BEFORE RETURNING, DELETE THE RECORD IN WSDLL.
     C*
     C     DLKEY         DELETE    PDL01                              59
     C*
     C                   MOVE      'B'           QQRTCD
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ELSE
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*****************************
     C*   PROCESS HELP REQUESTS   *
     C*****************************
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   WRITE     SCR99
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C******************************
     C*   PROCESS REQUEST SCREEN   *
     C******************************
     C*
     C     SCR01T        TAG
     C*
     C*  WE HAVE PASSED ALL EDITS, PROCESS SCREEN DATA
     C*
     C                   MOVEL     SC40A1        DL40A1
     C                   MOVEL     SC40A2        DL40A2
     C                   MOVEL     SC40A3        DL40A3
     C                   MOVEL     SC40A4        DL40A4
     C                   MOVEL     SC40A5        DL40A5
     C*
     C     *IN50         IFEQ      '0'
     C                   UPDATE    PDL01
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
