     H* (C) Copyright Fiserv Insurance Solutions, 2006
     H/TITLE FIXFTXBT                                                                       *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H* ================================================================== *
     H* Created: 6/21/06  C: 132  Project: MC025 - 52497
     H* ================================================================== *
     F* Copy records from FXGTP if Table ID TXBT does not exist in FTGIP
     F* using Company number from user file Company Number.
     H* ================================================================== *
     FFTGIL     UF A E           K DISK
     FFTGTL     UF A E           k DISK    rename(pgt01:pgt02)
     FFXGTP     IF   E             DISK
     FSMUPL     IF   E           k DISK
     F*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D kkco#           s                   like(gico#)
     D kktbid          s                   like(gitbid)
     D kkkey           s                   like(gtkey)
     D kkusr           s                   like(upusr)
     D xxco#           s                   like(gico#)
     D xxtbid          s                   like(gitbid)
     D xxDESC          c                   Const('TEXAS TYPE OF BILL')
     D*
     DPSDS           ESDS                  Extname(SZQ1P)
     C
     C     giky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C*
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    gtkey
     D*
     C     upky          klist
     C                   kfld                    kkusr
     C*
     C                   eval      kkusr  = q1user
     C                   eval      kktbid = 'TXBT'
     C*
     C     upky          chain     pup01                              99
     C*
     C                   if        not *in99
     C                   eval      kkco# = upco#
     C                   else
     C                   goto      term
     C                   endif
     C*
      * Chain to see if Table exists.
     C     GIKY          chain     pgi01                              98
     C*
      * If no generic table then add it.
     C                   IF        *in98
      *
     C                   clear                   pgi01
      *
     C                   time                    sys_time_n
     C                   move      *date         sys_date
     C     *cymd         move      sys_date      gidt
     C                   move      sys_time_n    gitime
     C                   eval      giuser = q1user
     C                   eval      gipgm  =  q1pgm
      *
     C                   eval      gico#  = kkco#
     C                   eval      gitbid = kktbid
     C                   eval      gistat = 'A'
     C                   movel(P)  xxDESC        gidesc
     C                   movel(P)  'CODE'        gifn01
     C*
     C                   write     pgi01
     C*
     C                   read      pgt01                                  99
     C*
     C                   dow       not *in99
     C*
     C* Now build new table from one in PDSDATA
     C*
     C     gtky          chain     pgt02                              97
     C*
     C                   if        *IN97
     C*
     C                   time                    sys_time_n
     C                   move      *date         sys_date
     C     *cymd         move      sys_date      gtdt
     C                   move      sys_time_n    gttime
     C                   eval      gtuser = q1user
     C                   eval      gtpgm =  q1pgm
     C*
     C                   Z-ADD     KKCO#         gtco#
     C*
     C                   write     pgt02
     C*
     C                   endif
     C*
     C                   read      pgt01                                  99
     C*
     C                   enddo
      *
     C                   EndIf                                                  *IN98 = *on
     C*
     C     term          tag
     C                   eval      *inlr = *on
     C                   return
