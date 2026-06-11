     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      * *****************************************************************
      /TITLE FA206R - API to Get RRN of FMSMP AND FMFNP.
      * *****************************************************************
      *  Description:                                                   *
      *                                                                 *
      *    By sending the Company/Group from WMFNP,                     *
      *    retrieve the relative record number from the corresponding   *
      *    record in FMFNP.                                             *
      *                                                                 *
      *  Called From:                                                   *
      *    W7265R                                                       *
      *                                                                 *
      *  Calls To:                                                      *
      *    None                                                         *
      *                                                                 *
      *  Created:  02/14/00  C:62  Project:  MC151                      *
      *                                                                 *
      * *****************************************************************
      *  Revisions:                                                     *
      *                                                                 *
      * *****************************************************************
      *  INDICATOR USAGE:                                             *
      *    INDICATOR            FUNCTION DESCRIPTION                  *
      *                                                               *
      *    99 - GENERAL USAGE (CHAIN)                                 *
      *                                                               *
      * ***************************************************************
      *
     FFMSML     IF   E           K DISK
     F                                     INFDS(DBFDS1)
     FFMFNL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)

     D*
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@WCF#            3 0
     C                   PARM                    @@RRN1            9 0          SUBSCRIBER RRN
     C                   PARM                    @@RRN2            9 0          GROUP RRN
     C                   PARM                    @@RTCD            1
     C*
     C     *LIKE         DEFINE    FNCO#         KKCO#
     C     *LIKE         DEFINE    FNSUB#        KKSUB#
     C     *LIKE         DEFINE    FNFND         KKFND
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@WCF#        KKFND
     C*
     C* GET THE SUBSCRIBER #
     C*
     C     FNKY          CHAIN     PFN01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     FNSUB#        KKSUB#
     C                   Z-ADD     DBRRN2        @@RRN2
     C*
     C     SMKY          CHAIN     PSM01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   Z-ADD     DBRRN1        @@RRN1
     C*
     C                   ELSE
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVEL     'F'           @@RTCD
     C*
     C                   ENDIF
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   RETURN
