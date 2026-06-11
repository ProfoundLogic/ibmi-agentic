     H* (C) Copyright Progressive Data Solutions, Inc., 2003
    H*
    H/TITLE - Calendar Earned/Unearned Job Schedule Control Program
    H*********************************************************************
    H*  DESCRIPTION:                                                     *
    H*    Call Calendar Earned/Unearned Submitter from job scheduler.    *
    H*                                                                   *
    H*  Created: 08/05/03      C:136     PROJECT: 107CN                  *
    H*********************************************************************
     H* REVISIONS:                                                        *
    H*                                                                   *
    H* 05/13/09 C:136 Support: WA139 65431                               *
    H*                Add parm to call of W4901C                         *
    H*                                                                   *
    H*********************************************************************
      *
     D sys_date        s               d   datfmt(*usa)
      *
     C     *entry        plist
     C                   parm                    @@co#             3
     C                   parm                    @@outq           10
      *
     c                   move      *date         sys_date
     c     *usa0         move      sys_date      cur_date          8
     c                   move      cur_date      from_date         8
     c                   movel     '0101'        from_date
      *
     c                   call      'W4901C'
     c                   parm                    @@co#
     c                   parm      '000'         @@fnd             3
     c                   parm                    from_date
     c                   parm                    cur_date
     c                   parm                    cur_date
     c                   parm      *blank        @@outq
     c                   parm      'C'           @@type            1
     c                   parm      'B'           @@prnt            1
     c                   parm      '001'         @@fgrp            3
     c                   parm      '999'         @@tgrp            3
     c                   parm      *blank        @@prtd            1
     c                   parm      *blank        @@srst            1
     c                   parm      'B'           @@typo            1
      *** beg add ***** 05/13/09 ******************************************************
     c                   parm      ' '           @@call            1
      *** end add ***** 05/13/09 ******************************************************
      *
     c                   seton                                        lr
