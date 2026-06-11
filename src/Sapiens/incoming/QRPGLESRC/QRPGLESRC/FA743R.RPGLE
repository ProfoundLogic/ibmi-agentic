     H**********************************************************************
     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: FA743R
    H* DESCRIPTION: API - DETERMINE IF A BILL WAS UPLOADE FROM A VENDOR
     H**********************************************************************
     H* CREATED: 08/25/05 C: 079 PROJECT: 10MGP
     H**********************************************************************
     H* REVISIONS:
     H**********************************************************************
     FFAEOP     IF   E             DISK
     F*
     FFAESL7    IF   E           K DISK
     F*
     D*
     D psds          esds                  extname(szq1p)
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##rrn           s              9  0
     D @@rrn           s              9  0
     D @@upld          s              1
     D @@mode          s              1
     D @@stcd          s                   like(esstcd)
     D kkstcd          s                   like(esstcd)
     D kkco#           s                   like(eoco#)
     D kksub#          s                   like(eosub#)
     D kkfnd           s                   like(eofnd)
     D kkfyr           s                   like(eofyr)
     D kkcase          s                   like(eocase)
     D kkeob#          s                   like(eoeob#)
     D kkblst          s                   like(esblst)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     C     genky         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C*
     C     esky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkblst
     C                   kfld                    kkstcd
     C*
     C     esky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C     *entry        plist
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@stcd
     C                   parm                    @@upld
     C*
     C*=====================================================================
     C* MAIN PROGRAM
     C*=====================================================================
     C*
     C                   eval      ##rrn  = @@rrn
     C                   eval      @@upld = *blank
     C*
     C     ##rrn         chain     peo01                              99
     C*
     C                   if        *in99 = *off
     C*
     C                   exsr      loadkeys
     C     esky          chain     pes01                              99
     C                   if        *in99 = *off
     C     esky1         readpe    pes01                                  99
     C                   if        *in99 = *on
     C                   eval      @@upld = 'Y'
     C                   else
     C                   if        @@mode = 'P' and esblst = 'U'
     C     esky1         readpe    pes01                                  99
     C                   if        *in99 = *on
     C                   eval      @@upld = 'Y'
     C                   endif
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   endif
     C*
     C                   eval      *inlr = *on
     C                   return
     C*
     C*=====================================================================
     C* SUBROUTINE loadkeys
     C*=====================================================================
     C*
     C     loadkeys      begsr
     C                   eval      kkco#  = eoco#
     C                   eval      kksub# = eosub#
     C                   eval      kkfnd  = eofnd
     C                   eval      kkfyr  = eofyr
     C                   eval      kkcase = eocase
     C                   eval      kkeob# = eoeob#
     C                   eval      kkblst = 'P'
     C                   eval      kkstcd = @@stcd
     C                   endsr
