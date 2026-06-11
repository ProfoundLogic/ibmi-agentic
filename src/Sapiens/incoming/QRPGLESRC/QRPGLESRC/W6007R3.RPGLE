     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE W6007R3 - Calculate Batch Date
     H**************************************************************************
     H*     Program description:                                               *
     H*        This program determines the batch date based on the date and    *
     H*        time that the job is submitted.                                 *
     H*                                                                        *
     H* CALLED FROM:                                                           *
     H*   W6007CA and W6007CG                                                  *
     H* CALLS TO:                                                              *
     H*   none                                                                 *
     H*                                                                        *
     H* PROGRAMMER:  C:15                                                      *
     H* CREATED: 05/28/04               PROJECT: WC1198                        *
     H**************************************************************************
     H* REVISIONS:                                                             *
     H*                                                                        *
     H* 07/31/13  C:53  Redmine 3483                                           *
     H*                 Change cutoff time from 1700 to 1500                   *
     H*                                                                        *
     H* 03/07/08  C:61  Project 132WKL                                         *
     H*                 Return system date if clt132.                          *
     H*                                                                        *
     H**************************************************************************
     D*  Date/time data structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D
     D runDt8          s              8  0
     C*-------------------------------------------------------------------------
     C*  Entry parm list
     c*
     c     *entry        plist
     c                   parm                    qqrndt            8
     c*
     c                   time                    dsdttm
     c*
      ******* beg add ****** 03/07/08 *********************************
     c                   call      'RTVCLTID'
     c                   parm                    CLTID             3

     c                   select

     c                   when      cltid = '132'
     c                   move(p)   dsdate        qqrndt

     c                   other

      ******* end add ****** 03/07/08 *********************************
      *** beg chg ****** 07/31/13 *********************************
     c****************** if        dstime < 170000
     c                   if        dstime < 150000
      *** end chg ****** 07/31/13 *********************************
     c*
     c                   call      'SRPAST8'
     c                   parm      dsdate        runDt8
     c                   parm      1             numdays           5 0
     c                   parm                    err               1
     c
     c                   move(p)   runDt8        qqrndt
     c                   else
     c
     c                   move(p)   dsdate        qqrndt
     c                   endif
      ******* beg add ****** 03/07/08 *********************************

     c                   endsl
      ******* end add ****** 03/07/08 *********************************
     C*
     C                   MOVE      *ON           *INLR
