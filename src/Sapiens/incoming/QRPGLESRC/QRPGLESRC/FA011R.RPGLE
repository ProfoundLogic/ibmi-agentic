     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*******************************************************************
     H/TITLE FA011R - API to Access SMCOP for Company Name
     H*******************************************************************
     H*  Description:                                                   *
     H*                                                                 *
     H*    Using @@CO#, access SMCOP to obtain Company Name @@CONM.     *
     H*    Using @@FND, access FMFNP to obtain Fund Name @@FNAM.        *
     H*
     H* * * * * * * * * * * * IMPORTNAT * * * * * * * * * * * * * * *
     H*
     H* This API will be a company only API.  If fund name is neeed
     H* then use FA007R.
     H*
     H* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     H*                                                                 *
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
     H*  04/13/99 C:56 Support: MC095 #17568
     H*                Change this API to accept a company number
     H*                and return the company name.
     H*                                                                 *
     H*******************************************************************
     H*  Indicators:                                                    *
     H*                                                                 *
     H*    99 - General File Access                                     *
     H*******************************************************************
      *
     FSMCOL     IF   E           K DISK
     F*** BEG DEL *** 04/13/99 ****************************************
     F**********SMUPL   IF  E           K        DISK
     F**********FMFNL   IF  E           K        DISK
     F*** END DEL *** 04/13/99 ****************************************
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C*** BEG DEL *** 04/13/99 ****************************************
     C**********           PARM           @@FND
     C*** END DEL *** 04/13/99 ****************************************
     C                   PARM                    @@CONM
     C*** BEG DEL *** 04/13/99 ****************************************
     C**********           PARM           @@FNAM
     C**********           PARM           @@SUB#
     C**********           PARM           @@FYR
     C*** END DEL *** 04/13/99 ****************************************
     C*
     C     COKEY         KLIST                                                  COMPANY
     C                   KFLD                    @@CO#
     C*
     C*** BEG DEL *** 04/13/99 ****************************************
     C********** FNKEY     KLIST
     C**********           KFLD           UPCO#
     C**********           KFLD           UPSUB#
     C**********           KFLD           UPFND
     C**********
     C********** UPKEY     KLIST                           USER PROFILE
     C**********           KFLD           Q1USER
     C*** END DEL *** 04/13/99 ****************************************
     C*
     C     *LIKE         DEFINE    COCO#         @@CO#
     C     *LIKE         DEFINE    CONAME        @@CONM
     C*** BEG DEL *** 04/13/99 ****************************************
     C********** *LIKE     DEFN UPFND     @@FND
     C********** *LIKE     DEFN CONAME    @@FNAM
     C********** *LIKE     DEFN UPSUB#    @@SUB#
     C********** *LIKE     DEFN UPFYR     @@FYR
     C*
     C**********           MOVE *ZEROS    UPCO#
     C**********           MOVE *ZEROS    UPFND
     C**********           MOVE *ZEROS    UPFYR
     C**********           MOVE *ZEROS    UPSUB#
     C**********           MOVE *BLANKS   FNNAME
     C**********           MOVE *BLANKS   @@FNAM
     C*** END DEL *** 04/13/99 ****************************************
     C                   MOVE      *BLANKS       @@CONM
     C*
     C*** BEG DEL *** 04/13/99 ****************************************
     C********** UPKEY     CHAINPUP01                99
     C********** *IN99     IFEQ *OFF
     C********** COKEY     CHAINPCO01                99
     C********** *IN99     IFEQ *OFF
     C**********           MOVELCONAME    ##DATA    P
     C**********           CALL 'SRCENTER'
     C**********           PARM           ##DATA 80
     C**********           PARM 40        ##LENG  30
     C**********           MOVEL##DATA    @@CONM
     C********** FNKEY     CHAINPFN01                99
     C**********           ENDIF
     C**********           ENDIF
     C**********
     C**********           MOVE UPCO#     @@CO#
     C**********           MOVE UPFND     @@FND
     C**********           MOVELFNNAME    @@FNAM
     C**********           MOVE UPSUB#    @@SUB#
     C**********           MOVE UPFYR     @@FYR
     C*** END DEL *** 04/13/99 ****************************************
     C*
     C*** BEG ADD *** 04/13/99 ****************************************
     C     COKEY         CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        @@CONM
     C                   ENDIF                                                  *IN99 = *OFF
     C*** END ADD 04/13/99 ********************************************
     C                   RETURN
