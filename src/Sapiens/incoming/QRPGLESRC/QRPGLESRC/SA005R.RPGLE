     *  (C) Copyright Progressive Data Solutions, Inc., 1998
     H dftactgrp(*no) actgrp(*CALLER)
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  07/10/98  C:69  Project:                                     *
     H*                  Created program                              *
     H*****************************************************************
     Fsmcol     if   e           k disk
     Fsmupl     if   e           k disk
     *
     D psds          esds                  extname(szq1p)
     D ##data          s             80
     D ##leng          s              3  0
     D @@co#           s                   like(upco#)
     D @@conm          s                   like(coname)
     *
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@conm
     *
     C     cokey         klist                                                  COMPANY
     C                   kfld                    upco#
     *
     C     upkey         klist                                                  USER PROFILE
     C                   kfld                    q1user
     *
     *
     C                   eval      upco# = *zeros
     C                   eval      @@conm = *blanks
     *
     C     upkey         chain     pup01                              99
B001 C                   if        *in99 = *off
 001 C     cokey         chain     pco01                              99
B002 C                   if        *in99 = *off
 002 C                   movel(p)  coname        ##data
 002 C                   call      'SRCENTER'
 002 C                   parm                    ##data
 002 C                   parm      40            ##leng
 002 C                   movel     ##data        @@conm
E002 C                   endif
E001 C                   endif
     *
     C                   move      upco#         @@co#
     *
     C                   return
