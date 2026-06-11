     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4230R - Undo Reconciliation
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/13/01 C:61  PROJECT WC1020                                 *
     H*                Change verbiage on prompt screen.              *
     H*                                                               *
     H*  09/04/97 C:30 Project: 87WBL                                 *
     H*           Version 3.  Replace WMBAP with WMBIP & WSCKP.       *
     H**                                                             **
     H*****************************************************************
     FW4230F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *** BEGIN CHG *** 09/04/97 ********************************
     F****WMBAL1  IF  E           K        DISK
      *
      *  Bank Account Master file
     FWSCKL     IF   E           K DISK
      *
      *** END   CHG *** 09/04/97 ********************************
     FWMCSL     IF   E           K DISK

     D MG              S             60    DIM(2) CTDATA PERRCD(1)
     D* Screen Reference
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* External Program Data Structures
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
      *** BEGIN CHG *** 09/04/97 ***********************************************
     C**Key List for the Bank Account Master File
     C***
     C***        BAKEY     KLIST
     C***                  KFLD           XXCO#
     C***                  KFLD           XXACCT 20
      *
      *  Bank Account Master file
      *
     C     CKKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCACCT
      *
      *** END   CHG *** 09/04/97 ***********************************************
     C*
     C* Key List for the Check Summary Master File
     C*
     C     CSKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXACCT           20
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
     C* Get the User Parameter Record
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              61
     C     *IN61         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPCO#         XXCO#             3 0
     C*
     C* Get the Comapny Record
     C*
     C     COKEY         CHAIN     PCO01                              61
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C* Set-up and Initialize
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVEL(P)  QQPRDS        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C                   MOVE      *BLANKS       SCMSG
     C                   MOVE      *BLANKS       SCMSG2
     C*
     C* Load the Record Format Infomation for the First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C* Display All Formats
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C* Process All Command Keys
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* F3=EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* F10=SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     HFLD          IFEQ      'SCACCT'
     C*
      *** BEGIN CHG *** 09/04/97 *******************************************
     C*
     C                   MOVE      UPCO#         ##CO#             3
     C                   MOVE      *ZERO         ##FND             3
      *
     C                   CALL      'W2420R'                                     Bank Info search
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM      *BLANKS       ##ACCT           20
     C                   PARM      *ZERO         ##EFDT            8
     C                   PARM      'WC'          ##APPL            2
     C                   PARM      *BLANKS       ##CKTP            2
      *
     C     ##ACCT        IFEQ      *BLANK
     C                   MOVE      *BLANK        SCACCT
     C                   ELSE
     C                   MOVE      ##ACCT        SCACCT
     C                   ENDIF
      *
      *** END   CHG *** 09/04/97 *****************************************
     C*
     C********** QQRTC     IFEQ '*'
     C                   MOVE      '1'           *IN30
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* PROCESS HELP REQUESTS
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
     C* PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C* Get the User parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              61
     C     *IN61         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C*                    END
     C*
     C* Goto the Appropriate Screen
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C* Process Request Screen
     C*
     C     SCR01T        TAG
     C*
     C* Validate the Account Number
     C*
     C                   MOVEL     SCACCT        XXACCT
      *** BEGIN CHG *** 09/04/97 *************************************
     C********** BAKY      CHAINPBA01                60
      *
     C     CKKEY         CHAIN     PCK01                              60
      *
      *** END   CHG *** 09/04/97 *************************************
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
      *** BEGIN ADD *** 09/04/97 *************************************
      *
     C     CKCRUP        IFNE      'Y'
     C                   MOVE      *ON           *IN22
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      *** END   ADD *** 09/04/97 *************************************
      *
     C*
     C* IF UNDO ALREADY PERFORMED ON THE PREVIOUS RECONCILIATION,
     C* SEND ERROR MESSAGE.
     C*
     C     CSKEY         SETGT     PCS01                              60
     C                   READP     PCS01                                  60
     C     *IN60         IFEQ      '1'
     C     CSSTAT        OREQ      'I'
     C                   MOVE      MG(1)         SCMSG
     C                   MOVE      MG(2)         SCMSG2
     C                   GOTO      STRT
     C                   END
     C*
     C* Passed all Edits Call CLP to Print Report
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      SCACCT        QQACCT           20
     C                   CALL      'W4230C'
     C                   PARM                    QQCO#
     C                   PARM                    QQACCT
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
**
Undo was already performed on the previous reconciliation.
Press F3=Exit to exit option.
