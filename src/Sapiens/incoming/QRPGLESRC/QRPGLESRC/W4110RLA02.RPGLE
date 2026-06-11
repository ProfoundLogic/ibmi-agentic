     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4110RLA02 - ADDITIONAL INPUT PROGRAM
     H*****************************************************************
     H* PROGRAM DESCRIPTION                                           *
     H*                                                               *
     H* This program prompts the user for additional input necessary  *
     H* to print a department letter.                                 *
     H*                                                               *
     H*  PROGRAMMER: C:190
     H*                                                               *
     H*  CREATED:    01/14/08               PROJECT: 133WET           *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H*****************************************************************
     FW4110FL2  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDCLL     UF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMDEL     IF   E           K DISK
     FWMDOL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWDC2P     O    E             DISK

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D*   ADDITIONAL PROGRAM PARMS
     D                 DS
     D  PARMS                  1    136
     D  QQCO#                  1      3
     D  QQFND                  4      6
     D  QQREQ#                 7     11
     D  QQDPCD                12     16
     D  QQDCID                17     28
     D  QQDESC                29     56
     D  QQFLDR                57    136
     D                 DS
     D  XXCZIP                 1      9  0
     D  SC4ZP1                 1      5  0
     D  SC4ZP2                 6      9  0
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  CL20A2                 1     20
     D  XXDARA                 1      3
     D  XXDPH1                 4      6
     D  XXDPH2                 7     10
     D  XXMISC                11     20
     D OPTNM           C                   CONST('Additional Input -
     D                                     Screen')
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICS
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY FOR LETTER WORKFILE
     C*
     C     CLKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXREQ#
     C                   KFLD                    QQDPCD
     C                   KFLD                    QQFLDR
     C                   KFLD                    QQDCID
     C*
     C*  CLAIM MASTER KEY LIST
     C*
     C     CMKEY         KLIST
     C                   KFLD                    CLCO#
     C                   KFLD                    CLFND
     C                   KFLD                    CLFYR
     C                   KFLD                    CLFCLM
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  DEPT MASTER FILE
     C*
     C     DEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    QQDPCD
     C*
     C*  DEPT/DOC ID MASTER FILE
     C*
     C     DOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXDEPT
     C                   KFLD                    QQDPCD
     C                   KFLD                    QQFLDR
     C                   KFLD                    QQDCID
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    CLFND         XXFND
     C     *LIKE         DEFINE    CLREQ#        XXREQ#
     C     *LIKE         DEFINE    DEDEPT        XXDEPT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQPRMS          136
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
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C*  GET THE DEPT LETTER WORKFILE RECORD
     C*
     C     CLKEY         CHAIN     PCL01                              50
     C                   MOVE      *BLANKS       SC1A1
     C                   MOVE      *BLANKS       SC1A2
     C                   MOVE      *BLANKS       SC1A3
     C                   MOVE      *BLANKS       SC1A4
     C                   MOVE      *BLANKS       SC1A5
     C                   MOVE      *BLANKS       SC1A6
     C                   MOVE      *BLANKS       SC1A7
     C                   MOVE      *BLANKS       SC1A8
     C                   MOVE      *BLANKS       SC1A9
     C                   MOVE      *BLANKS       SC1A10
     C                   MOVE      *BLANKS       SC1A11
     C                   MOVE      *BLANKS       SC1A12
     C                   MOVE      *BLANKS       SC1A13
     C                   MOVE      *BLANKS       SC1A14
     C                   MOVE      *BLANKS       SC1A15
     C                   MOVE      *BLANKS       SC1A16
     C                   MOVE      *BLANKS       SC1A17
     C                   MOVE      *BLANKS       SC1A18
     C                   MOVE      *BLANKS       SC1A19
     C                   MOVE      *BLANKS       SC40A1
     C                   MOVE      *BLANKS       SC40A2
     C                   MOVE      *ZEROS        SCDT1
     C                   MOVE      *ZEROS        SCDT2
     C                   MOVE      *ZEROS        SC5N1
     C                   MOVE      *ZEROS        SC5N2
     C                   MOVE      *ZEROS        SC9N1
     C                   MOVE      *ZEROS        SC9N2
     C                   MOVE      *ZEROS        SC9N3
     C                   MOVE      *ZEROS        SC9N4
     C                   MOVE      *ZEROS        SC9N5
     C                   MOVE      *ZEROS        SC9N6
     C*
     C*  GET DEPT/DOCUMENT RECORD
     C*
     C     DOKEY         CHAIN     PDO01                              60
     C*
     C*  GET DEPT. RECORD
     C*
     C     DEKEY         CHAIN     PDE01                              60
     C*
     C*  GET CLAIM RECORD
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C*  GET CLAIMANT DEMOGRAPHIC RECORD
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C*
     C*   ASSIGN SIGN HEADER FIELDS
     C*
     C                   MOVEL     DEDPNM        SCDPNM
     C                   MOVEL     DEDPCD        SCDPCD
     C                   MOVEL     QQDCID        SCDCID
     C                   MOVEL     DODESC        SCDESC
     C                   MOVE      XXFND         SCFND
     C                   MOVE      FNNAME        SCNAME
     C                   MOVE      CLFYR         SCFYR
     C                   MOVE      CMCASE        SCFCL2
      *
     C     CDLSN         CAT(P)    ',':0         SCCLNM
     C                   CAT       CDFSN:1       SCCLNM
     C     CDMID         IFNE      *BLANK
     C                   CAT       CDMID:1       SCCLNM
     C                   CAT       '.':0         SCCLNM
     C                   END
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
     C*          BEFORE RETURNING, DELETE THE RECORD IN WDCLL.
     C*
     C     CLKEY         DELETE    PCL01                              59
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
     C*          BEFORE RETURNING, DELETE THE RECORD IN WDCLL.
     C*
     C     CLKEY         DELETE    PCL01                              59
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
     C                   END                                                    END CMDKEYS
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
     C*
     C                   GOTO      STRT
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
     C*  EFFECTIVE DATE MUST BE VALID IF ENTERED
     C*
     C     SCDT1         IFNE      *ZEROS
     C                   Z-ADD     SCDT1         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT1
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C*  SUPPLEMENTAL EARNINGS BEGIN DATE MUST BE VALID IF ENTERED.
     C*
     C     SCDT2         IFNE      *ZEROS
     C                   Z-ADD     SCDT2         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDT2
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*  WE HAVE PASSED ALL EDITS, PROCESS SCREEN DATA
     C*
     C                   MOVE      SC1A1         CL1A1
     C                   MOVE      SC1A2         CL1A2
     C                   MOVE      SC1A3         CL1A3
     C                   MOVE      SC1A4         CL1A4
     C                   MOVE      SC1A5         CL1A5
     C                   MOVE      SC1A6         CL1A6
     C                   MOVE      SC1A7         CL1A7
     C                   MOVE      SC1A8         CL1A8
     C                   MOVE      SC1A9         CL1A9
     C                   MOVE      SC1A10        CL1A10
     C                   MOVE      SC1A11        CL1A11
     C                   MOVE      SC1A12        CL1A12
     C                   MOVE      SC1A13        CL1A13
     C                   MOVE      SC1A14        CL1A14
     C                   MOVE      SC1A15        CL1A15
     C                   MOVE      SC1A16        CL1A16
     C                   MOVE      SC1A17        CL1A17
     C                   MOVE      SC1A18        CL1A18
     C                   MOVE      SC1A19        CL1A19
     C                   MOVEL     SC40A1        CL40A1
     C                   MOVEL     SC40A2        CL40A2
     C                   MOVE      SCDT1         CLDT1
     C                   MOVE      SCDT2         CLDT2
     C                   MOVE      SC5N1         CL5N1
     C                   MOVE      SC5N2         CL5N2
     C                   MOVE      SC9N1         CL9N1
     C                   MOVE      SC9N2         CL9N2
     C                   MOVE      SC9N3         CL9N3
     C                   MOVE      SC9N4         CL9N4
     C                   MOVE      SC9N5         CL9N5
     C                   MOVE      SC9N6         CL9N6
     C*
     C                   WRITE     PC201
     C* ---------------------------------------------------------------
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
