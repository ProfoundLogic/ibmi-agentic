     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7540R - PROMPT - MONIES PAID TO SELECTED PAYEE REPORT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/01/00 C:15 Support: WC065/24856                           *
     H*                Chgd the modifier on the prompt screen to an   *
     H*                alphanumeric field so the pgm can distinguish  *
     H*                between a blank entry and a zero entry.        *
     H*                                                               *
     H*  09/24/98 C:78 PROJECT: WC577                                 *
     H*                DISPLAY Q1PGM INSTEAD OF W7540F IN SCREEN.     *
     H*                                                               *
     H*****************************************************************
     F*
     F*****************************************************************
     F*                       File Specs                              *
     F*****************************************************************
     F*
     F*   Prompt Screen
     F*
     FW7540F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F*   Company Master File
     F*
     FSMCOL     IF   E           K DISK
     F*
     F*   User Parameter File
     F*
     FWMUPL     IF   E           K DISK
     F*
     F*   Fund Master File
     F*
     FWMFNL     IF   E           K DISK
     F*
     F*   Payee Master File
     F*
     FWMPDL     IF   E           K DISK
     F*

     D*****************************************************************
     D*                      Input Specs                              *
     D*****************************************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*****************************************************************
     C*                      Calculation Specs                        *
     C*****************************************************************
     C*
     C*   Key List for the Company File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   Key List for the User Parameter File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   Key List for the Fund File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*  Key List for the Payee File
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C     PDKEY1        KLIST
     C                   KFLD                    XXFEIN
     C*
     C     *LIKE         DEFINE    PDDT          XXFRDT
     C     *LIKE         DEFINE    PDDT          XXTODT
     C*
     C*  Entry Parameters List
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
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
     C     TOP           TAG
     C*
     C*  Get the User Parameters Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPFND         SCFND
     C*
     C*  Get the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  Get the Fund Record, Load Fund Name for Screen
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              21
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*
     C*   Load Basic Screen Information
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   Load the Record Format Information for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*   Display All Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C*
     C                   MOVE      '0'           *IN30
     C*
     C*   Process All Command Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C* =======================================================
     C*  F10 - SEARCH
     C* =======================================================
     C*
     C     *INKJ         IFEQ      '1'
      *
     C     HFLD          IFEQ      'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SCFRDT
     C                   ENDIF
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         SCTODT
     C                   ENDIF
     C*
     C                   MOVE      SCFEIN        QQFEIN
     C*** BEGIN DLT *** 11/01/00 **************************************
     C*******************  MOVE SCMOD     QQMOD
     C*** END DLT ***** 11/01/00 **************************************
     C                   MOVE      '2'           QQCALL
     C                   CALL      'W0030R'
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQRTCD            1
      *
     C     QQRTCD        CABEQ     'A'           TERM
      *
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C                   MOVE      *ON           *IN70
     C                   ELSE
     C                   MOVE      *ON           *IN71
     C                   ENDIF
      *
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD IFEQ 'SCFE
     C                   ENDIF                                                  *INKJ IFEQ '1'
     C*
     C*   Process Help Requests
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
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   Process Change of User Parameters
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  Get the User Parameter File
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*   Goto the Appropriate Screen
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   Process Request Screen
     C*
     C     SCR01T        TAG
     C*
     C*  The Provider Number Cannot be Equal to Zero
     C*
     C     SCFEIN        IFLE      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN DLT *** 11/01/00 **************************************
     C*  The Modifier Cannot be Less than Zero
     C**********
     C********** SCMOD     IFLT *ZERO
     C**********           MOVE '1'       *IN24
     C**********           GOTO STRT
     C**********           END
     C*** END DLT ***** 11/01/00 **************************************
     C*
     C*  Validate the Requested Payee Number
     C*
     C                   MOVE      SCFEIN        XXFEIN
     C                   MOVE      SCMOD         XXMOD
     C*
     C*  If Modifier is BLANK Chain with partial key (PDKEY1), ELSE,
     C*  chain with full key (PDKEY)
     C*
     C*** BEGIN CHG *** 11/01/00 **************************************
     C********** XXMOD     IFEQ 0
     C     SCMOD         IFEQ      *BLANKS
     C*** END CHG ***** 11/01/00 **************************************
     C     PDKEY1        CHAIN     PPD01                              40
     C                   ELSE
     C     PDKEY         CHAIN     PPD01                              40
     C                   END
     C*
     C     *IN40         IFNE      '0'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  Validate the From Date
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  Validate the To Date
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   (FROM DATE MUST BE LESS THEN TO DATE)
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
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*   Call the CLP to Submit the List
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFEIN        QQFEIN            9
     C                   MOVE      SCMOD         QQMOD             3
     C                   MOVE      SCFRDT        QQFRDT            8
     C                   MOVE      SCTODT        QQTODT            8
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      'D'           QQMODE            1
     C*
     C                   CALL      'W7540C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQOUTQ
     C                   PARM                    QQMODE
     C*
     C*  End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
