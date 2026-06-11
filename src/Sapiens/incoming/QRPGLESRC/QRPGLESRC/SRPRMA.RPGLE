     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SRPRMA - PGM TO CALL S/R PREMIUM API
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS PROGRAM WILL ALLOW THE S/R PREMIUM API TO BE CALLED     *
     H*  WITH CHARACTER PARAMETERS INSTEAD OF NUMERIC.                *
     H*                                                               *
     H*  CALLED BY:                                                   *
     H*                                                               *
     H*  CALLS TO:  SRPREM                                            *
     H*                                                               *
     H*  PROGRAMMER: C:65                                             *
     H*  CREATED:    04/29/08          PROJECT: CI019                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/24/24 C:300 Project: 146WKR (Prescient)                    *
     H*                Collateral Processing                          *
     H*                                                               *
     H* 04/22/14  C:295 PROJECT:  CI130                               *
     H*                 Discount information on the online version of *
     H*                 S/R Payroll worksheet.                        *
     H*                                                               *
     H* 08/07/13 C:136 SUPPORT: RED3542 (LCTA)                        *
     H*               -ADD PARM TO INDICATE S/R ONLINE IS IN PLAY     *
     H*               -CORRECTION FOR WC2048                          *
     H*                                                               *
     H* 06/17/08  C:65  PROJECT:  CI019                               *
     H*                 ADD PREVIOUS BALANCE AMOUNT.                  *
     H*                                                               *
     H*****************************************************************

     ****** Beg add ***** 04/22/14 *************************************
     d psds          esds                  Extname(szq1p)
     ****** End add ***** 04/22/14 *************************************

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQRRN             9
     C                   PARM                    QQRPMY            6
     C                   PARM                    QQTODT            8
     C                   PARM                    QQMAPR           11
     C                   PARM                    QQADLD           11
     C                   PARM                    QQPAYR           11
     C                   PARM                    QQPREM           11
     C                   PARM                    QQTAX            11
     C                   PARM                    QQPPRM           11
     C                   PARM                    QQPTAX           11
     C                   PARM                    QQFACT          143
     C*** BEGIN ADD ***  06/17/08  **************************************
     C                   PARM                    QQPREV           11
     C*** END ADD *****  06/17/08  **************************************
     C*** Beg Add ***** 04/22/14 ****************************************
     C                   PARM                    QQINFO           39
     C                   PARM                    QQHDG           270
     C                   PARM                    QQPRO           100
     C*** End Add ***** 04/22/14 ****************************************
     C*** Begin Add *** 06/24/24 *******************************************
     C                   PARM                    QQPREM2          11
     C                   PARM                    QQTAX2           11
     C                   PARM                    QQPPRM2          11
     C                   PARM                    QQPTAX2          11
     C                   PARM                    QQFACT2          48
     C                   PARM                    QQASMNT         267
     C                   PARM                    QQPRVASMNT       77
     C                   PARM                    QQCOLB           11
     C                   PARM                    QQPRCOLB         11
     C                   PARM                    QQPRVCOLBAL      11
     C*** End   Add *** 06/24/24 *******************************************
     C*
     C                   MOVE      QQRRN         @@RRN
     C                   MOVE      QQRPMY        @@RPMY
     C                   MOVE      QQTODT        @@TODT
     C                   MOVE      QQMAPR        @@MAPR
     C                   MOVE      QQADLD        @@ADLD
     C                   MOVE      QQPAYR        @@PAYR
     C                   MOVE      QQPREM        @@PREM
     C                   MOVE      QQTAX         @@TAX
     C                   MOVE      QQPPRM        @@PPRM
     C                   MOVE      QQPTAX        @@PTAX
     C                   MOVE      QQFACT        @@FACT
     C*** BEGIN ADD ***  06/17/08  **************************************
     C                   MOVE      QQPREV        @@PREV
     C*** END ADD *****  06/17/08  **************************************
     C***** Beg Add ***** 04/22/14 ****************************************
     C     Q1PRMS        IFGE      15
     C                   MOVE      QQINFO        @@INFO
     C                   MOVE      QQHDG         @@HDG
     C                   MOVE      QQPRO         @@PRO
     C*** Begin Add *** 06/24/24 *******************************************
     C                   MOVE      QQPREM2       @@PREM2
     C                   MOVE      QQTAX2        @@TAX2
     C                   MOVE      QQPPRM2       @@PPRM2
     C                   MOVE      QQPTAX2       @@PTAX2
     C                   MOVE      QQFACT2       @@FACT2
     C                   MOVE      QQASMNT       @@ASMNT
     C                   MOVE      QQPRVASMNT    @@PRVASMNT
     C                   MOVE      QQCOLB        @@COLB
     C                   MOVE      QQPRCOLB      @@PRCOLB
     C                   MOVE      QQPRVCOLBAL   @@PRVCOLBAL
     C*** End   Add *** 06/24/24 *******************************************
     C*
     C                   CALL      'SRPREM'
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RPMY            6 0
     C                   PARM                    @@TODT            8 0
     C                   PARM                    @@MAPR           11 0
     C                   PARM                    @@ADLD           11 0
     C                   PARM                    @@PAYR           11 0
     C                   PARM                    @@PREM           11 0
     C                   PARM                    @@TAX            11 0
     C                   PARM                    @@PPRM           11 0
     C                   PARM                    @@PTAX           11 0
     C                   PARM                    @@FACT          143
     C*** BEGIN ADD ***  06/17/08  **************************************
     C                   PARM                    @@PREV           11 2
     C*** END ADD *****  06/17/08  **************************************
     C*** BEG ADD ***** 08/07/13 ****************************************
     C                   PARM      'S'           @@SRON            1
     C*** END ADD ***** 08/07/13 ****************************************
     C*
     C                   PARM                    @@INFO           39
     C                   PARM                    @@HDG           270
     C                   PARM                    @@PRO           100
     C*** Begin Add *** 06/24/24 *******************************************
     C                   PARM                    @@PREM2          11 2
     C                   PARM                    @@TAX2           11 2
     C                   PARM                    @@PPRM2          11 2
     C                   PARM                    @@PTAX2          11 2
     C                   PARM                    @@FACT2          48
     C                   PARM                    @@ASMNT         267
     C                   PARM                    @@PRVASMNT       77
     C                   PARM                    @@COLB           11 0
     C                   PARM                    @@PRCOLB         11 0
     C                   PARM                    @@PRVCOLBAL      11 0
     C*** End   Add *** 06/24/24 *******************************************
     C*
     C                   MOVE      @@INFO        QQINFO
     C                   MOVE      @@HDG         QQHDG
     C                   MOVE      @@PRO         QQPRO
     C*** Begin Add *** 06/24/24 *******************************************
     C                   MOVE      @@PREM2       QQPREM2
     C                   MOVE      @@TAX2        QQTAX2
     C                   MOVE      @@PPRM2       QQPPRM2
     C                   MOVE      @@PTAX2       QQPTAX2
     C                   MOVE      @@FACT2       QQFACT2
     C                   MOVE      @@ASMNT       QQASMNT
     C                   MOVE      @@PRVASMNT    QQPRVASMNT
     C                   MOVE      @@COLB        QQCOLB
     C                   MOVE      @@PRCOLB      QQPRCOLB
     C                   MOVE      @@PRVCOLBAL   QQPRVCOLBAL
     C*** End   Add *** 06/24/24 *******************************************
     C                   ELSE
     C*****End Add***** 04/22/14 ****************************************
     C                   CALL      'SRPREM'
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RPMY            6 0
     C                   PARM                    @@TODT            8 0
     C                   PARM                    @@MAPR           11 0
     C                   PARM                    @@ADLD           11 0
     C                   PARM                    @@PAYR           11 0
     C                   PARM                    @@PREM           11 0
     C                   PARM                    @@TAX            11 0
     C                   PARM                    @@PPRM           11 0
     C                   PARM                    @@PTAX           11 0
     C                   PARM                    @@FACT          143
     C*** BEGIN ADD ***  06/17/08  **************************************
     C                   PARM                    @@PREV           11 2
     C*** END ADD *****  06/17/08  **************************************
     C*** BEG ADD ***** 08/07/13 ****************************************
     C                   PARM      'S'           @@SRON            1
     C*** END ADD ***** 08/07/13 ****************************************
     C*
     C*****Beg Add***** 04/22/14 ****************************************
     C                   ENDIF
     C*****End Add***** 04/22/14 ****************************************
     C                   MOVE      @@RRN         QQRRN
     C                   MOVE      @@RPMY        QQRPMY
     C                   MOVE      @@TODT        QQTODT
     C                   MOVE      @@MAPR        QQMAPR
     C                   MOVE      @@ADLD        QQADLD
     C                   MOVE      @@PAYR        QQPAYR
     C                   MOVE      @@PREM        QQPREM
     C                   MOVE      @@TAX         QQTAX
     C                   MOVE      @@PPRM        QQPPRM
     C                   MOVE      @@PTAX        QQPTAX
     C                   MOVE      @@FACT        QQFACT
     C*** BEGIN ADD ***  06/17/08  **************************************
     C                   MOVE      @@PREV        QQPREV
     C*** END ADD *****  06/17/08  **************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
