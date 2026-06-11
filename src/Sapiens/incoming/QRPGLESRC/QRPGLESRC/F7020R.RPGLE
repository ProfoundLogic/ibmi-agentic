     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE F7020R - PROMPT FOR ICD-9-CM DIAGNOSIS CODE FILE LISTING
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program prints selected ICD9 codes with basic      **
     H**     information for each code                               **
     H**   CALLED FROM:                                              **
     H**     Menu                                                    **
     H**   CALLS TO:                                                 **
     H**     F0510R - ICD9 Numeric Search                            **
     H**     F7020C - ICD9 Listing Submitter CLP                     **
     H**   PROGRAMMER:                                               **
     H**   CREATED:                 PROJECT:                         **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                              *  *
     H*  05/20/09 C:33 Project: WC1770                               *  *
     H*                This program will now only call the system    *  *
     H*                 version.                                     *  *
     H*                                                               *
     H* 04/23/98  C:90  PROJECT: MC181                                *
     H*                 CHANGED F10 TO CALL F2010R INSTEAD            *
     H*                 OF F0510R                                     *
     H*                                                               *
     H* 04/07/98  C:89  PROJECT: MC181                                *
     H*                 CHANGED F6 TO F10 FOR SEARCH                  *
     H*                 VERSION 4 STANDARDS                           *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  20    MCR0702-FROM FIELD MUST BE LESS THAN TO FIELD          *
     H*  21    MCR0764-MUST PC AT ICD9 CODE FIELD FOR CODE SEARCH     *
     H*  22    PC AT FROM CODE                                        *
     H*  23    PC AT TO CODE                                          *
     H*****************************************************************
     FF7020F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK

     D*
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
     C***  Begin Add  ***  05/20/09  **************************************
     C                   CALL      'S7020R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      'MC'          ##AREA            2
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C***  End   Add  ***  05/20/09  **************************************
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
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
      **** BEGIN DEL **** 04/07/98 ************************************
     C******************** MOVE Q1USER    SCUSER
      **** END   DEL **** 04/07/98 ************************************
     C                   MOVE      QQPRDS        SCPRDS
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
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 03 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C***********************************
     C*   F10 =SEARCH                   *
     C***********************************
      **** BEGIN CHG **** 04/07/98 ************************************
     C********** *INKF     IFEQ '1'
     C     *INKJ         IFEQ      '1'
      **** END   CHG **** 04/07/98 ************************************
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
      **** BEGIN CHG **** 04/07/98 ************************************
     C******************** Z-ADD0         LIN     20
     C******************** Z-ADD0         COL     20
     C********** Q2CPOS    DIV  256       LIN
     C******************** MVR            COL     20
     C********************
     C********** LIN       IFNE 11
     C******************** MOVE '1'       *IN21
     C******************** GOTO STRT
     C******************** END
     C     HFLD          IFEQ      'SCFROM'
     C     HFLD          OREQ      'SCTO'
      **** END   CHG **** 04/07/98 ************************************
     C*
      **** BEGIN CHG **** 04/23/98 ************************************
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM      '    '        QQICD9            4
     C                   PARM                    QQLOST            4
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     HFLD          IFEQ      'SCFROM'
     C     *INKL         CABEQ     *ON           STRT                     22
     C                   ELSE
     C     *INKL         CABEQ     *ON           STRT                     23
     C                   ENDIF                                                  IFEQ SCFROM
      *
     C     HFLD          IFEQ      'SCFROM'
     C                   MOVE      QQICD9        SCFROM
     C     SCFROM        IFEQ      *BLANK
     C                   MOVE      *ON           *IN22
     C                   ELSE
     C                   MOVE      *ON           *IN23
     C                   ENDIF                                                  IFEQ *BLANK
     C                   ELSE
     C                   MOVE      QQICD9        SCTO
     C     SCTO          IFEQ      *BLANK
     C                   MOVE      *ON           *IN23
     C                   ELSE
     C                   MOVE      *ON           *IN22
     C                   ENDIF                                                  IFEQ *BLANK
     C                   ENDIF                                                  IFEQ SCFROM
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD SCFROM
      *
     C******************** MOVE *BLANK    QQICD9  5
     C******************** MOVELSCFROM    SCICD9  5
     C*
     C******************** CALL 'F0510R'
     C******************** PARM           QQPRDS
     C******************** PARM           QQCALL
     C******************** PARM           QQICD9
     C******************** PARM           QQRTC   1
     C*
     C******************** MOVE *BLANK    Q2RECF
     C******************** MOVEL'SCR01'   Q2RECF
     C*
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ 'B'
     C********** QQICD9    OREQ *BLANK
      **** BEGIN CHG **** 04/07/98 ************************************
     C********** COL       IFGE 36
     C********** COL       ANDLE40
     C*********  HFLD      IFEQ 'SCFROM'
      **** END   CHG **** 04/07/98 ************************************
     C******************** MOVE '1'       *IN22
     C******************** ELSE
     C*
      **** BEGIN CHG **** 04/07/98 ************************************
     C********** COL       IFGE 47
     C********** COL       ANDLE51
     C*********  HFLD      IFEQ 'SCTO'
      **** END   CHG **** 04/07/98 ************************************
     C******************** MOVE '1'       *IN23
     C******************** END                             END GE/LE
     C******************** END                             END GE/LE
     C*
     C******************** GOTO STRT
     C******************** END                             END EQ B/ZERO
     C*
     C********** QQRTC     IFEQ '*'
      **** BEGIN CHG **** 04/07/98 ************************************
     C********** COL       IFGE 36
     C********** COL       ANDLE40
     C*********  HFLD      IFEQ 'SCFROM'
      **** END   CHG **** 04/07/98 ************************************
     C******************** MOVE QQICD9    SCFROM
     C******************** MOVE '1'       *IN23
     C******************** ELSE
      **** BEGIN CHG **** 04/07/98 ************************************
     C********** COL       IFGE 47
     C********** COL       ANDLE51
     C*********  HFLD      IFEQ 'SCTO'
      **** END   CHG **** 04/07/98 ************************************
     C******************** MOVE QQICD9    SCTO
     C******************** MOVE '1'       *IN23
     C******************** END                             ENDIFCOL
     C******************** END                             ENDIFCOL
     C*
     C******************** GOTO STRT
     C******************** END                             END
     C*
      **** BEGIN ADD **** 04/07/98 ************************************
     C******************** ENDIF                           HFLD IFEQ 'SCFROM'
     C******************** MOVE '1'       *IN21
     C******************** GOTO STRT
      **** END   ADD **** 04/07/98 ************************************
     C                   END                                                    END 'SCR01'
     C*
     C                   END                                                    END *INKJ
     C***********************************
     C*  *IN10=PROCESS HELP REQUEST     *
     C***********************************
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
     C                   GOTO      STRT
     C                   END
     C***********************************
     C*   F13=CHG USER PARAMETERS       *
     C***********************************
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'F0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C****
     C                   END
     C****************************************************************
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*   SCFROM MUST BE LESS THAN OR EQUAL TO  SCTO
     C*
     C     SCTO          CABLT     SCFROM        STRT                   20
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         X@CO#             3
     C                   CALL      'F7020C'
     C                   PARM                    X@CO#
     C                   PARM                    SCFROM
     C                   PARM                    SCTO
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           QQMODE            1
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *====================================================================*
