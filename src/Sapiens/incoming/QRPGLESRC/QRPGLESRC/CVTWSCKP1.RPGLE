     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE C87WBL03
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION                                          *
     H*                                                               *
     H*    Initialize 'Voucher' and 'Check Rec' flag in "NEW"         *
     H*    Bank Account Master.                                       *
     H*                                                               *
     H*    Access "OLD" Bank Account Master.                          *
     H*    If found, assign Bank Account Name to "NEW" file field.    *
     H*                                                               *
     H*    Voucher flag   = 'N'                                       *
     H*    Check Rec flag = 'Y'                                       *
     H*                                                               *
     H*    After assignments made, update "NEW" Bank Account Master.  *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*  11/03/97 C:30 Project: V3 conversion of V1 level client.     *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*      60    READ/CHAIN Check Summary file WSCKL                *
     H*                                                               *
     H*****************************************************************
      *
      *** "New" Bank Account Master
     FWSCKP     UF   E           K DISK
      *
      *** "Old" Bank Account Master
     FWMBAL1    IF   E           K DISK
      *

     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     BAKY          KLIST
     C                   KFLD                    CKCO#
     C                   KFLD                    CKACCT
      *
      ****************************************************************
      *  MAIN LOOP
      ****************************************************************
      *
      *  Process "NEW" Bank Account Master
      *
     C                   READ      PCK01                                  60
      *
     C     *IN60         DOWEQ     '0'
      *
     C     CKACCT        IFEQ      'VOUCHER'
      *
     C                   DELETE    PCK01
      *
     C                   ELSE
      *
      *  Access "Old" Bank Account Master to obtain Bank Name
      *
     C     BAKY          CHAIN     PBA01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BANAME        CKAI01
     C                   ENDIF
      *
      *  Initialize flags
      *
     C                   MOVEL     'N'           CKVCHR                         VOUCHER
     C                   MOVEL     'Y'           CKCRUP                         CHECK REC UPDAT
      *
     C                   UPDATE    PCK01
     C                   ENDIF
      *
     C                   READ      PCK01                                  60
      *
     C                   ENDDO
      *
      * END PROGRAM
      *
     C                   SETON                                        LR
