     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* PURPOSE: PERFORM EDIT ON A VALID WCMS GROUP #
     H*
     H* CREATED: 06/15/99 C:53  PROJECT: MC145
     H*
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 10/13/00 C:15  Project: WC577                                 *
     H*                Added parms for the fund name and fund short   *
     H*                name.                                          *
     H*****************************************************************
     FWMFNL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#
     C                   PARM                    @@FND
     C                   PARM      *IN99         @@RTCD            1
     C*** BEGIN ADD *** 10/13/00 **************************************
     C                   PARM                    @@FNAM           60
     C                   PARM                    @@SHNM           30
     C*** END ADD ***** 10/13/00 **************************************
     C*
     C     FNKEY         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C*
     C     *LIKE         DEFINE    FNCO#         @@CO#
     C     *LIKE         DEFINE    FNFND         @@FND
     C*
     C     FNKEY         CHAIN     PFN01                              99
     C*** BEGIN ADD *** 10/13/00 **************************************
     C     *IN99         IFEQ      *OFF
     C     Q1PRMS        ANDGT     3
     C                   MOVEL     FNNAME        @@FNAM
     C                   MOVEL     FNSHNM        @@SHNM
     C                   ENDIF
     C*** END ADD ***** 10/13/00 **************************************
     C*
     C                   RETURN
