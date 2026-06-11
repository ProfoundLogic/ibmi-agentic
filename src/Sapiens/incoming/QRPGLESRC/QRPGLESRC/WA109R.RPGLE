     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - GET CO COORDINATOR CODE                                   WCMC
     H*
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: WA109R                                            *
     H*    CREATED: 09/30/98  C:94                                    *
     H*    DESCRIPTION: GET CO COORDINATOR CODE FROM WMEMP            *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FWMEML     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     *ENTRY        PLIST
      * INPUT FROM CALLER
     C                   PARM                    KKCO#                           I
     C                   PARM                    KKFND                           I
     C                   PARM                    KKEMP#                          I
     C                   PARM                    KKDIV                           I
      * RETURN TO CALLER
     C                   PARM                    XXPACC                          O
     C                   PARM                    XXRTCD            1             I/O
      *
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C     *LIKE         DEFINE    EMDIV         KKDIV
     C     *LIKE         DEFINE    EMPACC        XXPACC
      *
      * MAINLINE
     C     EMKY          CHAIN     PEM01                              99
     C                   MOVEL     EMPACC        XXPACC
     C*
      * PROGRAM TERMINATION
     C                   MOVE      '1'           *INLR
      /EJECT
