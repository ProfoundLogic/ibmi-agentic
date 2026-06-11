     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA041R - API to Access WMEML and Return RRN#
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*    This program will Access the WCMS Employer Master using    *
     H*    CO#/FND/EMP#/DIV key information.                          *
     H*    If found, the RRN# will be Returned.                       *
     H*    If NOT found, hival will be Returned.                      *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 08/13/99  PROGRAMMER: C:33  PROJECT: WC922          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FWMEML     IF   E           K DISK
     F                                     INFDS(EMFDS)
      *

     D EMFDS           DS
     D  EMRRN                397    400B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@EMP#            9
     C                   PARM                    @@DIV             5
     C                   PARM                    @@RRN#            9 0
     C*
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@EMP#        XXEMP#
     C                   MOVE      @@DIV         XXDIV
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EMRRN         @@RRN#
     C                   ELSE
     C                   Z-ADD     *HIVAL        @@RRN#
     C                   ENDIF
     C*
     C                   MOVE      *ON           *INLR
