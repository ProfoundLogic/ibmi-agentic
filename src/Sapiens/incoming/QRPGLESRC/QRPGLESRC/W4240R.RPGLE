     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4240R - BANK RECONCILIATION REPORT
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/13/01 C:61  PROJECT WC1020                                 *
     H*                Correct cursor position after return from      *
     H*                search.                                        *
     H*                                                               *
     H*  12/17/99 C:118 PROJECT: WC577                                *
     H*                 V3 DISPLAY FILE CHANGES.                      *
     H*                                                               *
     H*  02/13/97 C:30 Project: WC659                                 *
     H*           Version 3.  Replace WMBAP with WSCKP & WMBIP.       *
     H**                                                             **
     H*****************************************************************
     FW4240F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *** BEGIN CHG *** 02/13/97 ********************************
     F****WMBAL1  IF  E           K        DISK
      *
      *  Bank Information file
     FWMBIL4    IF   E           K DISK
      *
      *  Bank Account Master file
     FWSCKL     IF   E           K DISK
      *
      *** END   CHG *** 02/13/97 ********************************
     FWMCSL     IF   E           K DISK

     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
      *** BEGIN CHG *** 02/13/97 ***********************************************
     C***Key List for the Bank Account Master File
     C***
     C***        BAKEY     KLIST
     C***                  KFLD           XXCO#
     C***                  KFLD           XXACCT 20
      *
      *  Bank Information file
      *
     C     BIKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXAPPL            2
     C                   KFLD                    XXACCT           20
      *
      *  Bank Account Master file
      *
     C     CKKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCACCT
      *
      *** END   CHG *** 02/13/97 ***********************************************
     C*
     C* Key List for the Check Summary Master File
     C*
     C     CSKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C                   KFLD                    XXTODT
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
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
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
      *
      *** BEGIN ADD *** 02/13/97 ***********************************************
     C                   MOVE      *BLANK        SCACCT
      *** END   ADD *** 02/13/97 ***********************************************
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
      *** BEGIN DEL *** 11/19/97 *************************************
     C*
     C* F5=SEARCH LIST CALL
     C*
     C     *INKE         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
      *** BEGIN ADD *** 02/13/97 *************************************
     C     CKKEY         CHAIN     PCK01                              21
      *
     C     *IN21         IFEQ      *ON
     C     CKCRUP        ORNE      'Y'                                          CHECK REC FLAG
     C                   MOVE      *ON           *IN21
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      *** END   ADD *** 02/13/97 *************************************
      *
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
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C                   END
     C*
     C* F10=SEARCH
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C     HFLD          IFEQ      'SCACCT'
      *** BEGIN CHG *** 02/13/97 ***********************************************
     C*****                MOVE SCFND     QQFND   3
     C*****                CALL 'W4205R'
     C*****                PARM           QQPRDS
     C*****                PARM           QQCALL
     C*****                PARM           QQFND
     C*****                PARM           SCACCT
     C*****                PARM           QQRTC   1
     C*****      QQRTC     CABEQ'A'       TERM
     C*****
     C                   MOVE      UPCO#         ##CO#             3
     C                   MOVE      SCFND         ##FND             3
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
      ***** BEG ADD ******* 04/13/01 **********************************
     C                   SETON                                        30
      ***** END ADD ******* 04/13/01 **********************************
      *
     C                   ELSE
      *
     C                   MOVE      ##ACCT        SCACCT
      ***** BEG ADD ******* 04/13/01 **********************************
     C                   SETON                                        31
      ***** END ADD ******* 04/13/01 **********************************
     C                   ENDIF
      *
      *** END   CHG *** 02/13/97 ***********************************************
      ***** BEG ADD ******* 04/13/01 **********************************
     C********** QQRTC     IFEQ '*'
     C**********           MOVE '1'       *IN30
     C**********           END
      ***** END ADD ******* 04/13/01 **********************************
      *
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
      *** BEGIN CHG *** 02/13/97 *************************************
     C***********BAKEY     CHAINPBA01                60
      *
     C                   MOVE      'WC'          XXAPPL
      *
     C     BIKEY         CHAIN     PBI01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKEY         READE     PBI01                                  60
     C                   ENDDO
      *
      *** END   CHG *** 02/13/97 *************************************
      *
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
      *** BEGIN ADD *** 02/13/97 *************************************
     C     CKKEY         CHAIN     PCK01                              21
      *
     C     *IN21         IFEQ      *ON
     C     CKCRUP        ORNE      'Y'
     C                   MOVE      *ON           *IN21
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
      *** END   ADD *** 02/13/97 *************************************
      *
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
     C*   A CHECK SUMMARY RECORD MUST EXIST FOR THE ENTERED DATE
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     CSKEY         CHAIN     PCS01                              23
     C     *IN23         IFEQ      '0'
     C     CSSTAT        ANDEQ     'I'
     C                   MOVE      '1'           *IN23
     C                   END
     C     *IN23         CABEQ     '1'           STRT
     C*
     C* Passed all Edits Call CLP to Print Report
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      UPOUTQ        QQQUE            10
     C                   MOVE      SCACCT        QQACCT           20
     C                   MOVE      SCBAL         QQBAL             9
     C                   MOVE      SCTODT        QQTDT             8
     C                   CALL      'W4240C'
     C                   PARM                    QQCO#
     C                   PARM                    QQACCT
     C                   PARM                    QQBAL
     C                   PARM                    QQTDT
     C                   PARM                    QQQUE
     C*
     C* End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
