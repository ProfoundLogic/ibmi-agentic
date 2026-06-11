     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR**dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*                                                               *
     H* API - FA740R - Florida: Retrieve EOB X-ref                    *DATA
     H*******************************************************************
     H* Description:  Retrieves the EOB X-ref                         *D*TA
     H*                                                               * *
     H*  Called From: F0340R - EOMB Print                             * *
     H*               W7002R - Check Print                            * *
     H*  Calls To:    None                                            * *
     H*                                                               * *
     H*******************************************************************
     H* Created on 06/03/2004 by c:62/79 for  Project FL046           * *
     H*******************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     FFTABLFL   IF   E           K DISK

     C*================================================================
     C* Parameter lists
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@EOB
     C                   PARM                    @@XREF
     C                   PARM                    @@DESC
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
     C     *LIKE         DEFINE    EBDESC        @@DESC
     C     *LIKE         DEFINE    EBCO#         KKCO#
     C     *LIKE         DEFINE    EBKEY         KKKEY
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVEL(P)  @@EOB         KKKEY
     C     EBKY          CHAIN     PEB01                              99
     C     *IN99         IFEQ      *OFF
     C     EBXREF        IFNE      *BLANKS
     C                   MOVEL(P)  EBXREF        @@XREF
     C                   ELSE
     C                   MOVEL(P)  'S20'         @@XREF
     C                   ENDIF
     C*
     C     @@XREF        IFNE      'S20'
     C*
     C                   MOVEL(P)  @@XREF        KKKEY
     C     EBKY          CHAIN     PEB01                              99
     C*
     C                   MOVEL(P)  EBDESC        @@DESC
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   RETURN
