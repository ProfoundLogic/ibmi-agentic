     H* (C) Copyright Progressive Data Solutions, 2001
     H/TITLE FIX29552                                                                       *
     H**************************************************************************************
     H*  Description:  Fix mra in FMPRPAL                                                  *
     H*                                                                                    *
     H*  Created: 11/08/01   Programmer C:62    Project:MC071 29552                        *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFmprl2al  UF   E           K DISK
     D*
     d kkco#           s                   like(prco#)
     d kkefdt          s                   like(prefdt)
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0

     DPSDS           ESDS                  Extname(SZQ1P)

     C     *entry        plist
     C                   parm                    @@co#             3

     C     prky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkefdt

     C*
     C*----------------------------------------------------------------------
     C*  Read through FAEOP file
     C*----------------------------------------------------------------------
     c                   move      @@co#         kkco#
     C                   eval      kkefdt = 1010801
     C     prky1         chain     ppr01                              99
     C                   dow       (*in99 = *off)

     C                   if        prcpt4 >= '97001' and prcpt4 <= '97770'
     C                             and prprof <> 0
     C                   eval      prmra = prprof
     C                   endif
     C                   if        prcpt4 = '90901' and prprof <> 0
     C                   eval      prmra = prprof
     c                   endif
     C                   update    ppr01

     C     prky1         reade     ppr01                                  99
     C                   enddo                                                  *in60=*off
     C*
     C                   eval      *inlr=*on
