     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR**dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*                                                               *
     H* API - FA738R - Texas: Retrieve EOB X-ref                      *DATA
     H*******************************************************************
     H* Description:  Retrieves the EOB X-ref                         *D*TA
     H*                                                               * *
     H*  Called From: F0340R - EOMB Print                             * *
     H*               W7002R - Check Print                            * *
     H*  Calls To:    None                                            * *
     H*                                                               * *
     H*******************************************************************
     H* Created on 07/23/2003 by c:62 for Project TX022               * *
     H*******************************************************************
     H* Revisions:
     H* 02/18/05 C:79 PROJECT: TX029
     H*               USE EBXRF2 INSTEAD OF EBXREF(GROUP CODE)
     H*
     H*****************************************************************
     FFTABLTX   IF   E           K DISK

     C*================================================================
     C* Parameter lists
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@EOB
     C                   PARM                    @@XREF
     C*================================================================
     C* Key lists
     C*================================================================
     C     EBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*================================================================
     C* Like Defines
     C*================================================================
     C     *LIKE         DEFINE    EBCO#         @@CO#
     C     *LIKE         DEFINE    EBXREF        @@EOB
     C     *LIKE         DEFINE    EBXREF        @@XREF
     C     *LIKE         DEFINE    EBCO#         KKCO#
     C     *LIKE         DEFINE    EBKEY         KKKEY
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   MOVEL(P)  @@EOB         KKKEY
     C     EBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C*** BEG CHG *** 02/18/05 ****************************************
     C******************** MOVELEBXREF    @@XREF    P
     C                   MOVEL(P)  EBXRF2        @@XREF
     C*** END CHG *** 02/18/05 ****************************************
     C                   ENDIF
     C*
     C                   RETURN
