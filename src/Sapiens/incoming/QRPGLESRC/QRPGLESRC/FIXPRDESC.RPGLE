     H* (C) Copyright Progressive Data Solutions, 2000
     H/TITLE FIXCACHK#                                                                      *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H**************************************************************************************
     H*  Description:  Clear CACHK# for pended charges current recons                      *
     H*                                                                                    *
     H*  Created: 03/06/00   Programmer C:79    Project:MCMNT                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFmprlne   uf   E           K DISK
     FFmprlal   IF   E           K DISK    prefix(p2:2)
     F                                     rename(ppr01:ppr02)
     FQSYSPRT   O    F  198        printer

     D*
     d kkco#           s                   like(prco#)
     d kkcpt4          s                   like(prcpt4)
     d kkefdt          s                   like(prefdt)
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0

     DPSDS           ESDS                  Extname(SZQ1P)


     C     prky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkcpt4
     C                   kfld                    kkefdt

     C     prky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkcpt4

     C                   except    header
     C*
     C*----------------------------------------------------------------------
     C*  Read through FAEOP file
     C*----------------------------------------------------------------------
     C                   read      ppr01                                  99
     C                   dow       (*in99 = *off)
     C*
     C                   eval      kkco#=prco#
     C                   eval      kkcpt4=prcpt4
     C                   eval      kkefdt=9999999
     C*
     C     prky1         setgt     ppr02
     c     prky2         readpe    ppr02                                  98
     C                   if        (*in98 = *off)
     C                   eval      prdsc1=p2dsc1
     C                   eval      prdsc2=p2dsc2
     C                   update    ppr01
     c                   else
     c                   update    ppr01
     C*
     C                   if        *inof=*on
     C                   except    header
     C                   move      *off          *inof
     C                   endif
     C                   except    detail
     c                   endif

     C                   read      ppr01                                  99
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   eval      *inlr=*on
     Oqsysprt   e            header      5  1
     O                                           12 'cpt4'
     O                                           30 'eff date'
     o                                           43 'description'
     O          e            detail
     O                       prcpt4              12
     O                       prefdt        j     30
     o                       prdsc1              82
     o                       prdsc2             132
