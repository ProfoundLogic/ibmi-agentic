     H* (C) Copyright Progressive Data Solutions, 2003
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/TITLE WA050R - Check Mailing Date API
     H**************************************************************************************
     H*  Description:  This API retrieves the check mailing date from the Check Schedule   *
     H*                File based on the group and transaction date passed into the pgm.   *
     H*                                                                                    *
     H*  Called from: F6201R                                                               *
     H*  Calls to:                                                                         *
     H*                                                                                    *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created:  8/19/03    Programmer C:15    Project:WC1137                            *
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H* 01/13/10 C:227 Project: 139MNL                                                     *
     H*                Check mailing date                                                  *
     H*                                                                                    *
     H* 08/24/06 C:163 Project: 48WWM                                                      *
     H*                Check mailing date                                                  *
     H*                                                                                    *
     H**************************************************************************************
     Fwacpl     if   e           k disk

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s              3
     D @@fnd           s              3
     D @@trdt          s              8
     D @@cmdt          s              8
     D kkco#           s                   like(cpco#)
     D kkfnd           s                   like(cpfnd)
     D kkprdt          s                   like(cpprdt)
     D isodt           s               d   datfmt(*iso)
     D usadt           s               d   datfmt(*usa)
     D xxmdyy          s              8  0
     D xxyymd          s              8  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     C     cpky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkprdt
     C                   kfld                    kkfnd
     C*
     C*** BEG ADD *** 08/24/06 ****************************************
     C     cpky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkprdt
     C*** END ADD *** 08/24/06 ****************************************
     C*____________________________________________________________________
     C*
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@trdt
     C                   parm                    @@cmdt
     C*____________________________________________________________________
     C*
     C                   move      @@co#         kkco#
     C                   move      @@fnd         kkfnd
     C                   move      @@trdt        xxmdyy
     C     *usa          move      xxmdyy        isodt
     C                   move      isodt         xxyymd
     C     xxyymd        sub       19000000      kkprdt
     C                   clear                   @@cmdt
     C*
     C     cpky          chain     pcp01                              99
     C*
     C*** BEG ADD *** 08/24/06 ****************************************
     C                   if        *in99
     C     cpky1         chain     pcp01                              99
     C*** BEG ADD *** 01/13/10 ****************************************
     C                   endif
     C*** END ADD *** 01/13/10 ****************************************
     C*** END ADD *** 08/24/06 ****************************************
     C                   if        not *in99 and cpcmdt > 0
     C     *cymd         move      cpcmdt        isodt
     C                   move      isodt         usadt
     C                   move      usadt         xxmdyy
     C                   move      xxmdyy        @@cmdt
     C                   endif
     C*** BEG ADD *** 08/24/06 ****************************************
     C*** BEG DEL *** 01/13/10 ****************************************
     C***********        endif
     C*** END DEL *** 01/13/10 ****************************************
     C*** END ADD *** 08/24/06 ****************************************
     C*
     C*____________________________________________________________________
     C*
     C                   move      *on           *inlr
