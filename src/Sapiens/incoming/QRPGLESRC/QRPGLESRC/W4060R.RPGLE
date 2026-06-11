     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*
     H/TITLE W4060R - Prompt for Fund Years - 4 YR A/R Recap Report
     H***************************************************************
     H* REVISIONS:
     H*                                                               *
     H* 05/14/24  C:300 Project: WC2948                               *
     H*                 This program is no longer in use.             *
     H*                 Pass-through this program to new scree n.     *
     H*                 Add extra entry parm to avoid Pass-through of old screen
     H*
     H* 12/16/99  C:90  PROJECT: WC577
     H*                 Y2K FIX - PROPERLY DISPLAY RUN DATE.
     H*
     H* 09/10/99  C:118 PROJECT: WC577
     H*                 Program name reference chgd. Modified code
     H*                 so group name will clear if group number
     H*                 cleared.
     H*
     H* 08/21/95  C:28  PROJECT: WC445
     H*                 ADDED "INCLUDE FEE TRANSACTIONS (Y/N) FLAG
     H*                 TO PROMPT SCREEN.
     H***************************************************************
     F*
     F* Prompt Screen
     FW4060F    CF   E             WORKSTN
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
     F* Fund Master File
     FWMFNL     IF   E           K DISK
     F*
     F* Fund Detail File
     FWDF1L     IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  SCRNDT                 1      8  0
     D  XXMMDD                 1      4  0
     D  XXC19                  5      6  0
     D  XXYR                   7      8  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Comapnay Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Fund Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C* Key List for the Fund Detail File
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
      **** Begin Add **** 05/14/24 ******************************************
     C                   PARM                    @@PassThru        1
      **** End   Add **** 05/14/24 ******************************************
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

      **** Begin Add **** 05/14/24 ******************************************
      * Do not Pass-Through to new screen, if 3rd parameter (@@PassThru) is passed
     c                   If        Q1PRMS < 3
     c                   Call      'WTAR4YRCAP'
      *
      /free
       *InLr = *On;
       Return;
      /end-free
     C                   Endif
      **** End Add ****** 05/14/24 ******************************************
     C*
     C* Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C* Get the Company Record
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR1
     C*
     C* Get the Requested Record, Load Information
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 09/10/99 ***********************************
     C     *IN60         IFEQ      *OFF
     C***** END   ADD **** 09/10/99 ***********************************
     C                   MOVEL     FNNAME        SCNAME
     C***** BEGIN ADD **** 09/10/99 ***********************************
     C                   ELSE
     C                   MOVE      *BLANKS       SCNAME
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 09/10/99 ***********************************
     C*
     C                   TIME                    DSDTTM
      **** BEGIN CHG **** 12/16/99 ************************************
     C******************   MOVE DSDATE    XXYR
     C*******************  MOVELDSDATE    XXMMDD
     C*******************  Z-ADD19        XXC19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
      **** BEGIN CHG **** 12/16/99 ************************************
     C***** BEGIN DEL **** 09/10/99 ***********************************
     C**********           MOVE Q1USER    SCUSER
     C***** END   DEL **** 09/10/99 ***********************************
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C* Load the Record Format Info for First Time
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C                   MOVE      'N'           SCTRFE
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
     C* COMMAND 01 = End of Job
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* Process Help Requests
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
     C* Process Change of User Parameters
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C* Get the User Parameter Record
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
     C* Process Request Screen
     C*
     C     SCR01T        TAG
     C*
     C* Validate Fund Year 1 Entered - Fund Year 1 Must be Entered.
     C*
     C     SCFYR1        IFLE      0
     C                   MOVE      '1'           *IN20
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     SCFYR1        XXFYR
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C* Validate Fund Year 2 Entered
     C*
     C     SCFYR2        IFNE      0
     C                   Z-ADD     SCFYR2        XXFYR
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* Validate Fund Year 3 Entered
     C*
     C     SCFYR3        IFNE      0
     C                   Z-ADD     SCFYR3        XXFYR
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* Validate Fund Year 4 Entered
     C*
     C     SCFYR4        IFNE      0
     C                   Z-ADD     SCFYR4        XXFYR
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C* Validate the Current Balance Date Entered
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C* Validate Transaction Fee selection
     C*
     C     SCTRFE        IFNE      'N'
     C     SCTRFE        ANDNE     'Y'
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C* Call the CLP to Submit the List
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR1        QQFYR1            3
     C                   MOVE      SCFYR2        QQFYR2            3
     C                   MOVE      SCFYR3        QQFYR3            3
     C                   MOVE      SCFYR4        QQFYR4            3
     C                   MOVE      SCRNDT        QQRNDT            8
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      'D'           QQMODE            1
     C                   CALL      'W4060C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR1
     C                   PARM                    QQFYR2
     C                   PARM                    QQFYR3
     C                   PARM                    QQFYR4
     C                   PARM                    QQRNDT
     C                   PARM                    QQOUTQ
     C                   PARM                    QQMODE
     C                   PARM                    SCTRFE
     C*
     C*  End the Program
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
