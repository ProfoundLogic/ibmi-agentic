     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4225R - Reconciliation Report and File Update
     H************************************************************
     H*  REVISIONS:                                             **
     H*                                                         **
     H*  09/04/97 C:30 Project: WC659                           **
     H*           Version 3.  Replace WMBAP with WMBIP.         **
     H*                                                         **
     H************************************************************
     F*
     F* Prompt Screen
     FW4225F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FWMUPL     IF   E           K DISK
     F*
      *** BEGIN CHG *** 09/04/97 ********************************
     F**Bank Account Master File
     F****WMBAL1  IF  E           K        DISK
      *
      * Bank Account Master File
     FWSCKL     IF   E           K DISK
      *** END   CHG *** 09/04/97 ********************************
     F*
     F* Check Summary Master File
     FWMCSL     IF   E           K DISK

     D*
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
     C*****
     C*****      BAKEY     KLIST
     C*****                KFLD           XXCO#
     C*****                KFLD           XXACCT 20
      *
      *  Bank Account Master file
      *
     C     CKKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT           20
      *
      *** END   CHG *** 09/04/97 ***********************************************
     C*
     C* Key List for the Check Summary Master File
     C*
     C     CSKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C*
     C     *LIKE         DEFINE    CSDT          XXTODT
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
     C                   MOVE      QQPRDS        SCPRDS
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
     C* COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 05 = SEARCH LIST CALL
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      SCACCT        QQACCT
     C                   MOVE      *BLANKS       QQDATE
     C                   CALL      'W4235R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQACCT
     C                   PARM                    QQDATE            8
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQDATE        OREQ      *BLANKS
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      '*'
     C                   WRITE     SCR99
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
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
     C                   WRITE     SCR99
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
     C                   WRITE     SCR99
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
     C                   END
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
     C***********BAKEY     CHAINPBA01                60
      *
     C     CKKEY         CHAIN     PCK01                              60
      *
      *** END   CHG *** 09/04/97 *************************************
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate Ending Date
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C* THERE MUST NOT BE AN ACTIVE CHECK SUMMARY RECORD ON OR AFTER
     C* THE ENTERED ENDING DATE.
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     CSKEY         SETGT     PCS01
     C                   READP     PCS01                                  60
     C     *IN60         DOWEQ     '0'
     C     CSSTAT        ANDEQ     'I'
     C                   READP     PCS01                                  60
     C                   END
     C*
     C     *IN60         IFEQ      '0'
     C     CSACCT        ANDEQ     XXACCT
     C     CSCO#         ANDEQ     XXCO#
     C*
     C* A summary record exists after the entered date
     C*
     C     CSEDAT        IFGE      XXTODT
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* Passed all Edits Call CLP to Print Report
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      UPOUTQ        QQQUE            10
     C                   MOVE      SCACCT        QQACCT           20
     C                   MOVE      SCTODT        QQTDT             8
     C                   CALL      'W4225C'
     C                   PARM                    QQCO#
     C                   PARM                    QQACCT
     C                   PARM                    QQTDT
     C                   PARM                    QQQUE
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
