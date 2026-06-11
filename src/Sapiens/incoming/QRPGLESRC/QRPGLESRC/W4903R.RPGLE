     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4903R - CALENDAR EARNED/UNEARNED GROUP RANGE PROCESS
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM CALLS THE CALENDAR EARNED/UNEARNED REPORT FOR  *
     H*   EACH GROUP IN THE RANGE.                                    *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W4901C                                                      *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   W4903C                                                      *
     H*                                                               *
     H* PROGRAMMER: C:35                                              *
     H* CREATED: 02/28/01               PROJECT: WC577
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/16/09 C:136 SUPPORT: WA136 70300                           *
     H*                ADD SOME BACKWARDS COMPATILITY IN IF CALLED    *
     H*                FROM ANOTHER PROCESS                           *
     H*                                                               *
     H* 04/22/09 C:136 SUPPORT: WA136 65431                           *
     H*                ADD LOGIC FOR RUNNING CURRENT OR BETA VERSION  *
     H*                OF THE CEU                                     *
     H*                                                               *
     H* 10/22/02 C:142 PROJECT WC1094                                 *
     H*                ADDED PARMS FOR STATE SORT AND TYPE OF OUTPUT  *
     H*                                                               *
     H* 07/25/01 C:61  PROJECT WC577                                  *
     H*                Ignore TEST groups when group range entered.   *
     H*****************************************************************
     FWMFNL     IF   E           K DISK
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *   KEY LIST FOR THE FUND MASTER FILE
      *

     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     FNKY1         KLIST
     C                   KFLD                    XXCO#
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQVLDT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQPRNT            1
     C                   PARM                    QQFGRP            3
     C                   PARM                    QQTGRP            3
     C                   PARM                    QQPRTD            1
      ******* BEGIN ADD 10/22/02 **************************************
     C                   PARM                    QQSORT            1
     C                   PARM                    QQOUT             1
      ******* END   ADD 10/22/02 **************************************
      *** BEG ADD ***** 04/22/09 **************************************
     C                   PARM                    QQCALL            1
      *** END ADD ***** 04/22/09 **************************************
      **** BEGIN ADD **** 10/22/02 ************************************
     C     Q1PRMS        IFLT      12
     C                   MOVE      ' '           QQSRST            1
     C                   ELSE
     C                   MOVE      QQSORT        QQSRST
     C                   MOVE      QQOUT         QQTYPO            1
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
      *** BEG ADD ***** 10/16/09 **************************************
     C     Q1PRMS        IFGE      14
     C                   MOVE      QQCALL        XXCALL            1
     C                   ELSE
     C                   MOVE      *BLANKS       XXCALL
     C                   ENDIF
      *** END ADD ***** 10/16/09 **************************************
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      ******* BEGIN ADD 10/22/02 **************************************
     C     Q1PRMS        IFGT      11
     C                   ENDIF
      ******* END   ADD 10/22/02 **************************************
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFGRP        XXFND             3 0
     C                   MOVE      QQTGRP        XXTGRP            3 0
      *
     C     FNKY          SETLL     PFN01
     C     FNKY1         READE     PFN01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     FNFND         ANDLE     XXTGRP
      *
     C                   MOVE      FNFND         QQFND
      *
      ***** BEG ADD ******* 07/25/01 **********************************
     C     XXFND         IFEQ      XXTGRP
     C     FNTEST        ORNE      'Y'
      ***** END ADD ******* 07/25/01 **********************************
      *
     C                   CALL      'W4903C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQVLDT
     C                   PARM                    QQQUE
     C                   PARM                    QQTYPE
     C                   PARM                    QQPRNT
     C                   PARM                    QQFGRP
     C                   PARM                    QQTGRP
     C                   PARM                    QQPRTD
      ******* BEGIN ADD 10/22/02 **************************************
     C                   PARM                    QQSRST
     C                   PARM                    QQTYPO
      ******* END   ADD 10/22/02 **************************************
      *** BEG ADD ***** 04/22/09 **************************************
      *** BEG CHG ***** 10/16/09 **************************************
     C*****************    PARM           QQCALL  1
     C                   PARM                    XXCALL            1
      *** END CHG ***** 10/16/09 **************************************
      *** END ADD ***** 04/22/09 **************************************
      *
      ***** BEG ADD ******* 07/25/01 **********************************
     C                   ENDIF                                                  XXFND EQ XXTGRP
      ***** END ADD ******* 07/25/01 **********************************
      *
     C     FNKY1         READE     PFN01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ *OFF
      *
     C                   MOVE      *ON           *INLR
