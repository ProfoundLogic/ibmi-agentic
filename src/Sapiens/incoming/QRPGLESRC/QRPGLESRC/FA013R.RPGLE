     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*******************************************************************
     H/TITLE FA011R - API to Access SMCOP, SMUPP, & FMFNP
     H*******************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*    Using @@CO#, access SMCOP to obtain Company Name @@CONM.     *
     H*    Using @@FND, access FMFNP to obtain Fund Name @@FNAM.        *
     H*                                                                 *
     H*  Called From:                                                   *
     H*                                                                 *
     H*                                                                 *
     H*  Calls To:                                                      *
     H*    None                                                         *
     H*                                                                 *
     H*  Created:  04/11/97  C:32  Project:  87MBN                      *
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
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@FND
     C                   PARM                    @@CONM
     C                   PARM                    @@FNAM
     C                   PARM                    @@SUB#
     C                   PARM                    @@FYR
     C*
     C     COKEY         KLIST                                                  COMPANY
     C                   KFLD                    UPCO#
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    UPFND
     C*
     C     UPKEY         KLIST                                                  USER PROFILE
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    UPCO#         @@CO#
     C     *LIKE         DEFINE    UPFND         @@FND
     C     *LIKE         DEFINE    CONAME        @@CONM
     C     *LIKE         DEFINE    CONAME        @@FNAM
     C     *LIKE         DEFINE    UPSUB#        @@SUB#
     C     *LIKE         DEFINE    UPFYR         @@FYR
     C*
     C                   MOVE      *ZEROS        UPCO#
     C                   MOVE      *ZEROS        UPFND
     C                   MOVE      *ZEROS        UPFYR
     C                   MOVE      *ZEROS        UPSUB#
     C                   MOVE      *BLANKS       FNNAME
     C                   MOVE      *BLANKS       @@CONM
     C                   MOVE      *BLANKS       @@FNAM
     C*
     C     UPKEY         CHAIN     PUP01                              99
     C     *IN99         IFEQ      *OFF
     C     COKEY         CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        @@CONM
     C     FNKEY         CHAIN     PFN01                              99
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      UPCO#         @@CO#
     C                   MOVE      UPFND         @@FND
     C                   MOVEL     FNNAME        @@FNAM
     C                   MOVE      UPSUB#        @@SUB#
     C                   MOVE      UPFYR         @@FYR
     C                   RETURN
