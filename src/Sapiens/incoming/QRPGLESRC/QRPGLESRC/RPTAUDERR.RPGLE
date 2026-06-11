     Hcopyright('(c) Copyright StoneRiver, Inc., 2009')
     Hdftactgrp(*no) option(*nodebugio:*srcstmt)
     H*====================================================================
     H/TITLE RPTAUDERR - Report Audit Errors
     H*====================================================================
     H* Created by C:136 on 02/25/09
     H*====================================================================
     fwdell     if   e           k disk
     fqfeul1    if   e           k disk
     fwdehl6    if   e           k disk    rename(peh01:peh06)
     frptaudfil uf a e           k disk

     d qqrcym          s              5
     d kkrcym          s                   like(EURCYM)
     d errCode         s                   like(ADCODE)
     d errVbg          s                   like(ADERR)
     d totalWritten    s                   like(EUYDWR)
     d totalEarned     s                   like(EUYDWR)
     d kktrdt          s                   like(EHTRDT)

     c     euky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELFYR
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    kkrcym

     c     ehky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c     ehky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR
     c                   kfld                    kktrdt

     c     *entry        plist
     c                   parm                    qqrcym

     c                   read      pad01
     c                   dow       not(%eof)
     c                   delete    pad01
     c                   read      pad01
     c                   enddo

     c                   movel     qqrcym        kkrcym
     c                   movel     qqrcym        kktrdt
     c                   move      '01'          kktrdt
     c                   read      wdell
     c                   dow       not(%eof)
     c                   if        ELFAF = 'A'
     c                   eval      totalWritten = 0
     c                   eval      totalEarned = 0
     c                   eval      errvbg = *blanks
     c                   eval      errCode = *blanks
     c     euky          setll     peu01
     c     euky          reade     peu01
     c                   dow       not(%eof)
     c                   eval      totalWritten =
     c                             totalWritten + EUYDWR + EUPYWR
     c                   eval      totalEarned  =
     c                             totalEarned  + EUYDEA + EUPYEA
     c     euky          reade     peu01
     c                   enddo
     c     ehky1         setll     peh06
     c     ehky          readpe    peh06
     c                   if        not(%eof)
     c                   select
     c                   when      ELAAPR <> totalWritten
     c                   eval      totalWritten = totalWritten + ELPRTD
     c                   if        ELAAPR <> totalWritten
     c                   eval      totalWritten = totalWritten - ELPRTD
     c                   eval      errCode = '01'
     c                   eval      errvbg = 'CEU written differs from Audit.'
     c                   endif
     c                   when      ELAAPR <> totalEarned
     c                   eval      totalEarned = totalEarned + ELPRTD
     c                   if        ELAAPR <> totalEarned
     c                   eval      totalEarned = totalEarned - ELPRTD
     c                   eval      errCode = '02'
     c                   eval      errvbg = 'Written vs Earned difference.'
     c                   endif
     c                   endsl
     c                   else
     c                   eval      errCode = '03'
     c                   eval      errvbg = 'No premium history exists.'
     c                   endif
     c                   if        errCode <> *blanks
     c                   exsr      writeOut
     c                   endif
     c                   endif
     c                   read      wdell
     c                   enddo

     c                   seton                                        lr
      *=================================================================================
      * Write Out -  writes to RptAudFil
      *=================================================================================
     c     writeOut      begsr
     c                   clear                   pad01
     c                   eval      ADCO# = ELCO#
     c                   eval      ADFND = ELFND
     c                   eval      ADEMP# = ELEMP#
     c                   eval      ADDIV = ELDIV
     c                   eval      ADFYR = ELFYR
     c                   eval      ADRCYM = kkrcym
     c                   eval      ADELWR = ELAAPR
     c                   eval      ADEUWR = totalWritten
     c                   eval      ADEUEA = totalEarned
     c                   eval      ADCODE = errCode
     c                   eval      ADERR  = errVbg
     c                   write     pad01
     c                   endsr
