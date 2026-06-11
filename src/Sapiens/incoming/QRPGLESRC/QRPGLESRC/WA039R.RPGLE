     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - WA039R - CHECK PAYMENTS ON A BILL                        DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: WA039R                                            *
     H*    CREATED: 07/01/99  C:56  PROJECT: MC202                    *
     H*    DESCRIPTION: DETERMINE IF A BILL HAS ANY WCMS PAYMENTS     *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*  5/26/17 C: 079 Support: 90/47213                             *
     H*                 Do not return Y if the check has been voided  *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       99      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FWDPALG    IF   E           K DISK

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  WORKER'S COMP PAYMENT DETAIL FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** Beg Add *** 05/26/17 ****************************************
     C                   kfld                    kkRcn
     C*** End Add *** 05/26/17 ****************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EOB#            9 0
     C*** Beg Add *** 05/26/17 ****************************************
     C                   parm                    @@rcn             3 0
     C*** End Add *** 05/26/17 ****************************************
     C*
     C     *LIKE         DEFINE    PACO#         KKCO#
     C     *LIKE         DEFINE    PAFND         KKFND
     C     *LIKE         DEFINE    PAFYR         KKFYR
     C     *LIKE         DEFINE    PACASE        KKCASE
     C     *LIKE         DEFINE    PAEOB#        KKEOB#
     C*** Beg Add *** 05/26/17 ****************************************
     C     *like         define    PAeobS        KKrcn
     C*** End Add *** 05/26/17 ****************************************
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C                   Z-ADD     @@EOB#        KKEOB#
     C*** Beg Add *** 05/26/17 ****************************************
     C                   z-add     @@Rcn         kkRcn
     C*** End Add *** 05/26/17 ****************************************
     C*
     C* CHECK TO SEE IF A PAYMENT HAS BEEN MADE FOR THIS BILL
     C* AND ITS NOT VOIDED
     C*
     C     PAKY          CHAIN     PPA01                              99
     C*** Beg Chg *** 05/26/17 ****************************************
     C**** *IN99         IFEQ      *OFF
     C                   if        *in99 = *off and
     C                             (PAadSt <> 'V' and PAtrcd <> 'VO' and
     C                             PAtype <> 'V')
     C*** End Chg *** 05/26/17 ****************************************
     C                   MOVE      'Y'           @@RTCD
     C                   ELSE
     C                   MOVE      'N'           @@RTCD
     C                   ENDIF                                                  &UB99 = *OFF
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   RETURN
     C*
