     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      * *****************************************************************
      /TITLE FA201R - API to Get RRN of FMCMP from Corresponding WMCMP
      * *****************************************************************
      *  Description:                                                   *
      *                                                                 *
      *    By sending the Company/Group/Year/Case from WMCMP,           *
      *    retrieve the relative record number from the corresponding   *
      *    record in FMCMP.                                             *
      *                                                                 *
      *  Called From:                                                   *
      *    W0070R                                                       *
      *                                                                 *
      *  Calls To:                                                      *
      *    None                                                         *
      *                                                                 *
      *  Created:  11/20/97  C:47  Project:  60APD                      *
      *                                                                 *
      * *****************************************************************
      *  Revisions:                                                     *
      *                                                                 *
      *  06/18/01 C:62 PROJECT: MC082 26153                             *
      *                SEND FAIL RETURN CODE IF CAN'T GET RRN.          *
      *                                                                 *
      * *****************************************************************
      *  INDICATOR USAGE:                                             *
      *    INDICATOR            FUNCTION DESCRIPTION                  *
      *                                                               *
      *    60 - 61    GENERAL USAGE (CHAIN)                           *
      *                                                               *
      * ***************************************************************
      *
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS1)
     FFMFNL1    IF   E           K DISK

     D*
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  CLAIM MASTER KEY LIST
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQRRN             9 0
     C                   PARM                    QQRTCD            1
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMSUB#        XXSUB#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C*
     C* GET THE SUBSCRIBER #
     C*
     C     FNKY          CHAIN     PFN01                              61
     C     *IN61         IFEQ      *OFF
     C                   Z-ADD     FNSUB#        XXSUB#
     C                   Z-ADD     FNFND         XXFND
     C                   END
     C*
     C* GET THE CORRESPONDING RRN
     C*
     C                   Z-ADD     *ZERO         QQRRN
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRN1        QQRRN
     C*** BEGIN ADD *** 06/18/01 **************************************
     C                   ELSE
     C                   MOVEL     'F'           QQRTCD
     C*** END   ADD *** 06/18/01 **************************************
     C                   END
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   RETURN
