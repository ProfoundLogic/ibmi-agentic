     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*caller)
      ***************************************************************
      * (C) Copyright Progressive Data Solutions, Inc., 2005       **
      *                                                            **
      * Program:     SC853R1                                       **
      * Description: Policy Selection API by Policy Year           **
      *                                                            **
      * Created By:  C:167  Proj:129IMD   Date:  12/28/05          **
      ***************************************************************
      * REVISIONS:                                                 **
      *                                                            **
      ***************************************************************
      * INDICATOR USAGE:                                           **
      *  60                    CHAIN/DOWEQ                         **
      ***************************************************************
     FWDELL     IF   E           K DISK
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  PLYKEY                 1     40
     D  XXCO#                  1      3
     D  XXFND                  4      6
     D  XXEMP#                 7     15
     D  XXDIV                 16     20
     D  XXREST                21     40
      *
     D @@FYR           s              3
     D @@RTN           s              1
     D ##FYR           s              3  0 inz(0)
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     C     KEYPLY        plist
     C                   parm                    PLYKEY
     C                   parm                    @@RTN
      *
      *  ENTRY PARM LIST ***
     C     *entry        plist
     C                   parm                    @@FYR
     C                   parm                    @@RTN
      *
     C                   move      @@FYR         ##FYR
     C     *loval        setll     PEL01
     C                   read      PEL01                                  60
     C                   dow       *in60 = *OFF
     C                   if        elfyr <= ##FYR
     C                   move      ELCO#         XXCO#
     C                   move      ELFND         XXFND
     C                   move      ELEMP#        XXEMP#
     C                   move      ELDIV         XXDIV
     C                   call      'SC853R3'     KEYPLY
     C                   endif
     C                   read      PEL01                                  60
     C                   enddo
      *
      * end the program
      *
     C                   return
