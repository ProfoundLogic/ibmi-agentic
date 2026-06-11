     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA026R - API to Access WDCRP / Return CHK# & Activity DT
     H* ***************************************************************
     H*  DESCRIPTION:                                                 *
     H*    This program will access the Check Reconciliation file for *
     H*    a given Key and return Check # and Activity Date.          *
     H*                                                               *
     H*  CREATED: 10/20/97  C:30  Project:  WC660                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*
     H*****************************************************************
      * Check Reconciliation
     FWDCRL     IF   E           K DISK
      *__________________________________________________________________
      *

     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *  Check Rec key
      *

     C     CRKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
      *
      *  *LIKE Definitions
      *
     C     *LIKE         DEFINE    CRCO#         KKCO#
     C     *LIKE         DEFINE    CRKY          KKKEY
      *
      *  Date Conversion
      *
     C     DTMDCY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*MDCY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *__________________________________________________________________
      *  Program Start
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3            Company
     C                   PARM                    KKKEY                          Group #
     C                   PARM                    @@CHK#            7            Applic Code
     C                   PARM                    @@ACTP            1            Activity Type
     C                   PARM                    @@ACDT            8            Activity MMDDCCYY
     C                   PARM                    @@RTCD            1            Return Code
      *
     C                   MOVE      @@CO#         KKCO#                          Company
      *
      *  Access Check Rec file to obtain Check # and Activity Date
      *
     C     CRKEY         CHAIN     PCR01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CRCHK#        @@CHK#                         Check #
     C                   MOVE      CRACTP        @@ACTP                         Activity Type
     C                   MOVE      *BLANK        @@RTCD                         Return Code
      *
     C                   Z-ADD     CRACDT        SRDTI                          CYYMMDD
     C                   CALL      'SRCVTDAT'    DTMDCY
     C                   MOVEL     SRDTO         @@ACDT                         MMDDCCYY
      *
     C                   ELSE
      *
     C                   MOVE      *BLANK        @@RTCD                         Return Code
     C                   MOVE      *ZERO         @@CHK#                         Check #
     C                   MOVE      *BLANK        @@ACTP                         Activity Type
     C                   MOVE      *ZERO         @@ACDT                         Activity Date
     C                   ENDIF
      *
     C                   RETURN
