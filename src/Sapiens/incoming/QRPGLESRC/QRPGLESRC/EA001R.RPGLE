     H* (C) Copyright Progressive Data Solutions, 2004
     H*
     H* EA001R
     H***************************************************************
     H* Retrieve values from EXXTP
     H***************************************************************
     H*
     H* Given:
     H*   co#, fnd, emp#, date (current date cymd)
     H*
     H* Return:
     H*   @@err = '0' if record found, otherwise return '1'
     H*   values for: xtexls, xtepls, xtexex, xtwvwv, xtball
     H*
     H***************************************************************
     H* REVISIONS:
     H*
     H* 12/09/04  C:61  support wa129 44713
     H*                 If prior record not found, seek most current.
     H*
     H* 06/16/04  C:61  PROJECT WC1173
     H*                 CREATED
     H***************************************************************

     * Employer Experiece Mod Work File
     Fexxtl1    if   e           k disk


     D@@co#            s              3  0
     D@@fnd            s              3  0
     D@@emp#           s              9  0
     D@@date           s              7  0
     D@@exls           s              9  0
     D@@epls           s              9  0
     D@@exex           s              9  0
     D@@wvwv           s              3  2
     D@@ball           s             11  0
     D@@err            s              1


      *================================================================
      * KLIST
      *================================================================

     *
     *  Employer Experience Mod Work File
     *
     C     xtky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@fnd
     C                   kfld                    @@emp#
     *
     C     xtky1         klist
     C                   kfld                    @@co#
     C                   kfld                    @@fnd
     C                   kfld                    @@emp#
     C                   kfld                    @@date
     *


      *================================================================
      * PLIST
      *================================================================

     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@emp#
     C                   parm                    @@date
     C                   parm                    @@exls
     C                   parm                    @@epls
     C                   parm                    @@exex
     C                   parm                    @@wvwv
     C                   parm                    @@ball
     C                   parm                    @@err


     c                   clear                   pxx01
     c     xtky1         setgt     pxx01
     c     xtky          readpe    pxx01                                  60

      ******* beg add ********* 12/09/04 ****************************
     c                   if        *in60 = *on
     c     xtky          chain     pxx01                              60
     c                   endif
      ******* end add ********* 12/09/04 ****************************

     c                   eval      @@exls = xtexls
     c                   eval      @@epls = xtepls
     c                   eval      @@exex = xtexex
     c                   eval      @@wvwv = xtwvwv
     c                   eval      @@ball = xtball
     c                   eval      @@err = *in60


     C                   seton                                        lr

