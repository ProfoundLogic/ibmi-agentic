     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA004R - API to Access WMCML1 and Return RRN#
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*    This program will Access the WCMS Claim Master using       *
     H*    CO#/FND/FYR/CASE key information.                          *
     H*    If found, the RRN# will be Returned.                       *
     H*    If NOT found, zero will be Returned.                       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 05/20/97  PROGRAMMER: C:30  PROJECT: WC660          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
      *
      * MCMS Claim Master file
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS1)
      *

     D DBFDS1          DS
     D  DBRRN1               397    400B 0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *

     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
      *
      *================================================================
      *  PLIST definitions
      *================================================================
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@RRN#            9 0
      *
      *================================================================
      *  Normal Programming Area
      *================================================================
      *
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
      *
     C     CMKY          CHAIN     PCM01                              40
      *
     C     *IN40         IFEQ      *OFF
     C                   Z-ADD     DBRRN1        @@RRN#
     C                   ELSE
     C                   Z-ADD     *ZERO         @@RRN#
     C                   ENDIF
      *
     C                   MOVE      *ON           *INLR
