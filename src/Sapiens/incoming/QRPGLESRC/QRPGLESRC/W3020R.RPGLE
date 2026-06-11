     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W3020R - PROMPT FOR RUN DATE & COMM. PMT. CODE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  12/21/06 C:183  PROJECT: 114WND                              *
     H*                  Add a record to WMPHP.                       *
     H*                                                               *
     H*  12/01/99 C:33 SEQ#: WA108/20848                              *
     H*                CHANGED ERROR MESSAGE ON SCREEN (NO PGM CHGS)  *
     H*                                                               *
     H*  06/12/98 C:90 PROJECT WC577                                  *
     H*                CHANGE SCREENS TO CURRENT STANDARDS            *
     H*                                                               *
     H*  05/19/98 C:91 PROJECT WC577                                  *
     H*                CENTER SCREEN TITLE                            *
     H*                CHANGE SCREENS TO CURRENT STANDARDS            *
     H*                                                               *
     H*  12/02/97 C:30 Project:  WC500                                *
     H*           Add edit Group Aux Process Agent Commission Checks  *
     H*           must be 'Y' to allow Requests for Commission Checks.*
     H*                                                               *
     H*****************************************************************
     FW3020F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTABL     IF   E           K DISK
      *** BEGIN ADD *** 12/02/97 **************************************
      *
      *  Group Auxiliary file
     FWDF2L     IF   E           K DISK
     F***** BEG ADD *****  12/21/06  *********************************
     FWMPHP     O    E             DISK
     F***** END ADD *****  12/21/06  *********************************
      *** END   ADD *** 12/02/97 **************************************

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
     C*  COMMISSION RECORD CODE
     C*
     C     ACKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
      *
      *** BEGIN ADD *** 12/02/97 **************************************
      *
      *  Group Auxiliary file
      *
     C     F2KY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
      *
      *** END   ADD *** 12/02/97 **************************************
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
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
     C                   MOVE      UPFND         SCFND
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*  SET THE TIME AND DATE, LOAD INFORMATION
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCDATE
     C*
     C                   MOVE      *BLANK        SCCODE
     C*
      **** BEGIN ADD **** 05/19/98 ************************************
     C                   MOVEL(P)  SCPRDS        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
      **** END   ADD **** 05/19/98 ************************************
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
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 05 = SEARCH LIST CALL
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVEL     SCCODE        XX2               2
     C     XX2           IFEQ      '? '
     C     HFLD          OREQ      'SCCODE'
     C*
     C                   MOVE      *BLANK        CSKEY
     C                   CALL      'W2201R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    CSKEY
     C*
     C                   MOVEL     CSKEY         SCCODE
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     CSKEY         OREQ      *BLANK
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      **** BEGIN ADD **** 06/12/98 ************************************
     C                   MOVE      *ON           *IN26
      **** END   ADD **** 06/12/98 ************************************
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C                   END
     C*
      **** BEGIN ADD **** 06/12/90 ************************************
     C                   MOVE      *ON           *IN90
     C                   GOTO      STRT
      **** END   ADD **** 06/12/98 ************************************
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
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
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
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
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
      *
      *** BEGIN ADD *** 12/03/97 ***********************************************
      *
      *  Access Group Auxiliary file.
      *  Process Agent Commssions flag must be 'Y'.  Otherwise, display error.
      *
     C     F2KY          CHAIN     PF201                              60
      *
     C     F2AGCK        IFNE      'Y'
     C                   SETON                                        25
     C                   GOTO      STRT
     C                   ENDIF
      *
      *** END   ADD *** 12/03/97 ***********************************************
     C*
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE COMMISSION CODE
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCCODE        XXKEY
     C     ACKY1         CHAIN     PCS01                              40
     C     *IN40         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C     CSTYPE        IFNE      'P'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C     CSSTAT        IFEQ      'I'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
      ****BEG ADD *****  12/21/06  ************************************
     C                   CLEAR                   PPH01
     C                   MOVE      UPCO#         PHCO#
     C                   MOVE      SCFND         PHFND
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
      *
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI
     C                   PARM      '     '       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVEL     SRDTO         PHDT
      *
     C                   MOVE      DSTIME        PHTIME
     C                   MOVE      Q1USER        PHUSER
     C                   MOVE      Q1PGM         PHPGM
      *
     C                   Z-ADD     Q1DT          SRDTI
      *
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVEL     SRDTO         PHIDT
     C                   MOVE      Q1TIME        PHITIM
     C                   MOVE      Q1USER        PHIUSR
     C                   MOVE      Q1PGM         PHIPGM
     C                   MOVE      'R'           PHTYPE
     C                   MOVE      SCDATE        PHTODT
     C                   MOVE      SCCODE        PHCOCD
     C                   MOVEL     SCCODE        XXKEY            10
     C     ACKY1         CHAIN     PCS01                              40
     C     *IN40         IFEQ      '0'
     C                   MOVE      CSTYPE        PHCOTP
     C                   MOVE      CSDESC        PHCODS
     C                   ELSE
     C                   MOVE      ' '           PHCOTP
     C                   MOVE      ' '           PHCODS
     C                   ENDIF
     C                   WRITE     PPH01
      ****END ADD *****  12/21/06  ************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCDATE        QQDATE            8
     C                   MOVE      SCCODE        QQCODE            2
     C                   CALL      'W3020C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQDATE
     C                   PARM                    QQCODE
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
