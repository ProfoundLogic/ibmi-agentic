     H* (C) Copyright Progressive Data Solutions, 2002
     H/TITLE CVTMSCPT4                                                                      *
     H**************************************************************************************
     H*  Description:  Populate PR1A4, Add Gen Table items and flex heading                *
     H*                                                                                    *
     H*  Created: 10/11/02   Programmer C:79    Support: MC044 34328                       *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFMPRPFL   UF   E             DISK
     FFTGTL     UF A E           K DISK
     FFTGIL     UF A E           K DISK
     FFDCFL     UF   E           K DISK
     FSMCOP     IF   E             DISK
     FSMUPP     IF   E             DISK

     D*
     D desc            s             40A   dim(3) ctdata perrcd(1)
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D kkco#           s                   like(gico#)
     D kktbid          s                   like(gitbid)
     D kkkey           s                   like(gtkey)
     D kkst            s                   like(cfst)
     D ##co#           s                   like(upco#)
     D ##sub#          s                   like(upsub#)
     D ##fnd           s                   like(upfnd)
     D ##fyr           s                   like(upfyr)
     D ##conm          s                   like(coname)
     D ##fnam          s                   like(coname)
     D*
     DPSDS           ESDS                  Extname(SZQ1P)
     C*
     C     cfky          klist
     C                   kfld                    kkst
     C*
     C     giky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C*
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey
     C*
     C* Retrieve company number using API
     C*
     C                   exsr      rtvco#
     C*
     C* Populate PR1A4 with "Y" for codes not counted in PT rule
     C*
     C                   exsr      FMPRP
     C*
     C* Add generic table items for rule use
     C*
     C                   exsr      GENTBL
     C*
     C* Add flexible heading for SC1A5 in work with state codes for Florida
     C*
     C                   exsr      FDCFP
     C*
     C* Close files and return
     C*
     C                   eval      *inlr = *on
     C                   return
     C*
     C*=====================================================================
     C*    SUBROUTINE FMPRP
     C*=====================================================================
     C     fmprp         begsr
     C*
     C                   read      ppr01                                  99
     C*
     C                   dow       *in99 = *off
     C*
     C                   if        prcpt4 = '97150' or prcpt4 = '97504' or
     C                             prcpt4 = '97520' or prcpt4 = '97532' or
     C                             prcpt4 = '97533' or prcpt4 = '97542'
     C*
     C                   eval      pr1a4 = 'Y'
     C*
     C                   exsr      dttm
     C*
     C     *cymd         move      sys_date      prdt
     C                   eval      prpgm=q1pgm
     C                   eval      pruser=q1user
     C                   move      sys_time      prtime
     C*
     C                   update    ppr01
     C*
     C                   endif
     C*
     C                   read      ppr01                                  99
     C*
     C                   enddo
     C*
     C                   endsr
     C*
     C*=====================================================================
     C*    SUBROUTINE FDCFP
     C*=====================================================================
     C     fdcfp         begsr
     C*
     C                   eval      kkst = 'FL'
     C*
     C     cfky          chain     pcf01                              99
     C*
     C                   if        *in99 = *off
     C*
     C                   eval      cf1h5 = 'Excl from rule count'
     C                   eval      cf1s5 = 'Excl'
     C*
     C                   exsr      dttm
     C*
     C     *cymd         move      sys_date      cfdt
     C                   eval      cfpgm=q1pgm
     C                   eval      cfuser=q1user
     C                   move      sys_time      cftime
     C*
     C                   update    pcf01
     C*
     C                   endif
     C*
     C                   endsr
     C*
     C*=====================================================================
     C*    SUBROUTINE GENTBL
     C*=====================================================================
     C     gentbl        begsr
     C*
     C                   eval      kktbid = 'RLUS'
     C*
     C     giky          chain     pgi01                              99
     C*
     C                   if        *in99 = *on
     C*
     C                   eval      gico# = kkco#
     C                   eval      gistat = 'A'
     C                   eval      gifn01 = 'CODE'
     C                   eval      gitbid = 'RLUS'
     C                   eval      gidesc = 'STATE RULE USE'
     C*
     C                   exsr      dttm
     C*
     C     *cymd         move      sys_date      gidt
     C                   eval      gipgm=q1pgm
     C                   eval      giuser=q1user
     C                   move      sys_time      gitime
     C*
     C                   write     pgi01
     C*
     C                   exsr      dttm
     C*
     C     *cymd         move      sys_date      gtdt
     C                   eval      gtpgm=q1pgm
     C                   eval      gtuser=q1user
     C                   move      sys_time      gttime
     C*
     C                   eval      gtco# = kkco#
     C                   eval      gtstat = 'A'
     C                   eval      gttbid = gitbid
     C*
     C                   eval      gtkey = 'I'
     C                   eval      gtdesc = desc(1)
     C*
     C                   write     pgt01
     C*
     C                   eval      gtkey = 'E'
     C                   eval      gtdesc = desc(2)
     C*
     C                   write     pgt01
     C*
     C                   eval      gtkey = 'C'
     C                   eval      gtdesc = desc(3)
     C*
     C                   write     pgt01
     C*
     C                   endif
     C*
     C                   endsr
     C*
     C*=====================================================================
     C*    SUBROUTINE DTTM
     C*=====================================================================
     C     dttm          begsr
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C                   endsr
     C*
     C*=====================================================================
     C*    SUBROUTINE RTVCO#
     C*=====================================================================
     C     rtvco#        begsr
     C*
     C                   call      'FA007R'
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
     C                   parm                    ##sub#
     C                   parm                    ##fyr
     C*
     C                   eval      kkco# = ##co#
     C*
     C                   if        kkco# = 0
     C                   read(n)   ppr01                                  99
     C*
     C                   if        *in99 = *off
     C                   eval      kkco# = prco#
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
     C*
** DESC
INCLUDE ONLY FLAGGED CODES IN COUNT
EXCLUDE FLAGGED CODES IN COUNT
INCLUDE ONLY FLAGGED CODES IN CASCADING
