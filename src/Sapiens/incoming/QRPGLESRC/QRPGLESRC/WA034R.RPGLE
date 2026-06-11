     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA034R - API TO OBTAIN WCMC INFORMATION FOR AUTO/CHK ISS.
     H* ***************************************************************
     H*  DESCRIPTION:                                                 *
     H*    THIS PROGRAM WILL ACCESS WMCIP TO OBTAIN INFORMATION FOR   *
     H*    WCMC AUTO/CHECK ISSUANCE.                                  *
     H*                                                               *
     H*    PARM Description:                                          *
     H*         Input:                                                *
     H*           @@CO#   3        Company                            *
     H*           @@FND   3        Group #                            *
     H*           @@SYS   1        SYSTEM CODE (U,M)                  *
     H*         Output:                                               *
     H*           @@TRCD  2        WMCIP TRANSACTION CODE             *
     H*           @@STS   2        WMCIP CHECK STATUS CODE
     H*                                                               *
     H*  CREATED: 02/10/99  C:94  Project:  48MBV                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*
     H*****************************************************************
      * WCMC AUTO/CHK INFORMATION
     FWMCIL     IF   E           K DISK
      *__________________________________________________________________
      *

     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  Bank Account Master key
      *

     C     CIKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKTYPE
     C                   KFLD                    @@SYS
      *
      *  *LIKE Definitions
      *
     C     *LIKE         DEFINE    CICO#         KKCO#
     C     *LIKE         DEFINE    CIFND         KKFND
     C     *LIKE         DEFINE    CITRCD        KKTRCD
     C     *LIKE         DEFINE    CISTS         KKSTS
      *
      *__________________________________________________________________
      *  Program Start
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3            Company
     C                   PARM                    @@FND             3            Group #
     C                   PARM                    @@SYS             1            SYSTEM (U,M)
     C                   PARM                    @@TRCD            2            TRANSACTION CODE
     C                   PARM                    @@STS             2            CHECK STATUS
      *
     C                   MOVE      @@CO#         KKCO#                          Company
     C                   MOVE      @@FND         KKFND                          Group #
     C                   MOVE      *BLANKS       KKTRCD
     C                   MOVE      *BLANKS       KKSTS
     C*
     C                   MOVEL     'B'           KKTYPE            1
      *
     C     CIKY          CHAIN     PCI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      CITRCD        KKTRCD
     C                   MOVE      CISTS         KKSTS
     C                   MOVE      KKTRCD        @@TRCD
     C                   MOVE      KKSTS         @@STS
     C                   ENDIF
     C*
      *
     C                   RETURN
