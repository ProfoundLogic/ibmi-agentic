     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*caller)
      ***************************************************************
      * (C) Copyright Progressive Data Solutions, Inc., 2005       **
      *                                                            **
      * Program:     SC853R2                                       **
      * Description: Policy Selection API by Policy Year           **
      *                                                            **
      * Created By:  C:167  Proj:129IMD   Date:  12/27/05          **
      ***************************************************************
      * REVISIONS:                                                 **
      *                                                            **
      ***************************************************************
      * INDICATOR USAGE:                                           **
      *  60                    CHAIN/DOWEQ                         **
      ***************************************************************
     FWMCML1    IF   E           K DISK
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  CLMKEY                 1     40
     D  XXCO#                  1      3
     D  XXFND                  4      6
     D  XXFYR                  7      9
     D  XXCASE                10     16
     D  XXREST                17     40
      *
     D @@FYR           s              3
     D @@RTN           s              1
     D ##FYR           s              3  0 inz(0)
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     C     KEYCLM        PLIST
     C                   PARM                    CLMKEY
     C                   PARM                    @@RTN
      *
      *  ENTRY PARM LIST ***
     C     *entry        plist
     C                   parm                    @@FYR
     C                   parm                    @@RTN
      *
     C                   move      @@FYR         ##FYR
     C     *loval        setll     PCM01
     C                   read      PCM01                                  60
     C                   dow       *in60 = *OFF
     C                   if        cmfyr <= ##FYR
     C                   move      CMCO#         XXCO#
     C                   move      CMFND         XXFND
     C                   move      CMFYR         XXFYR
     C                   move      CMCASE        XXCASE
     C                   call      'SC853R3'     KEYCLM
     C                   endif
     C                   read      PCM01                                  60
     C                   enddo
      *
      * end the program
      *
     C                   return
