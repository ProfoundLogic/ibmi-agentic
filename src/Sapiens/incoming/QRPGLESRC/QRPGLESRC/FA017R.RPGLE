     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*******************************************************************
     H/TITLE FA017R - API to Retrieve Fund Auxiliary Data
     H*******************************************************************
     H*  Description:                                                   *
     H*    This API retrieves values from the Fund Auxiliary file.      *
     H*                                                                 *
     H*  Called From:                                                   *
     H*    W0080R                                                       *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  10/24/97  C:15  Project:                             *
     H*                                                                 *
     H*******************************************************************
     H*  Revisions:                                                     *
     H*                                                                 *
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    99 - General File Access                                     *
     H*******************************************************************
      *
     FFMFNL1    IF   E           K DISK
     FFMF1L     IF   E           K DISK
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@WCF#            3
     C                   PARM                    @@FLG2            1
     C                   PARM                    @@6N1             6
     C                   PARM                    @@RTCD            1
     C*
     C*** Key list for the WCMC Fund Master File
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C*
     C*** Key list for the WCMC Fund Auxiliary File
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C     *LIKE         DEFINE    FNWCF#        XXWCF#
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    FNSUB#        XXSUB#
     C*
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@WCF#        XXWCF#
     C                   MOVE      *ZEROS        XXFND
     C                   MOVE      *ZEROS        XXSUB#
     C*
     C*** Translate the WCMS fund into WCMC Sub# and fund.
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVE      FNSUB#        XXSUB#
     C                   MOVE      FNFND         XXFND
     C                   ENDIF
     C*
     C*** Retrieve the Fund Auxiliary data
     C*
     C     F1KY          CHAIN     PF101                              99
     C     *IN99         IFEQ      '0'
     C                   MOVE      F1FLG2        @@FLG2
     C                   MOVE      F16N1         @@6N1
     C                   ENDIF
     C*
     C     ENDPGM        TAG
     C                   RETURN
