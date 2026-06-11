     hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')

      /title - Query for zero payrolls written by S/R Online

     h option(*nodebugio:*srcstmt)
     h debug(*yes)
     h dftactgrp(*no) actgrp('QRYWASRP')
     h bnddir('QRYWASRP')

      * Lists months in which S/R payroll amounts generate premium amounts
      * different from S/R billings by > 10% in PDSFIL/QRYWASRP.

      *=========================================================================
     fwdell1    if   e           k disk    rename(pel01 : pel0l)
     f                                     infds(elrrnds)
     fwtabl     if   e           k disk
     fwdf1l     if   e           k disk    rename(pf101 : pf10l)
     fwmepl1e   if   e           k disk    rename(pep01 : pep0l1e)
     fwasrl5    if   e           k disk    rename(psr01 : psr0l5)
     fqrywasrp  uf a e             disk

     d psds          esds                  extname(SZQ1P)

     d elrrnds         ds
     d  dbrrn                397    400b 0
     d svco#           s              3  0
     d svfnd           s              3  0
     d kkkey           s             10
     d mthARBilling    s             11  2
     d sumSRPayAm      s             11  2
     d estManPrem      s             11  0
     d srManPrem       s             11  0
     d polRptMth       s              5  0
     d polExpMth       s              5  0
     d srRptMth        s              5  0
     d kkRptDt         s              7  0
     d workDate        s               d
     d cymdDate        s              7  0
     d x               s              2  0
     d y               s              2  0
     d elrrn           s              9  0
     d qqrrn           s                   like(ELRRN)
     d qqrpmy          s              6  0
     d qqtodt          s              8  0
     d qqmapr          s                   like(estManPrem)
     d qqadld          s                   like(estManPrem)
     d qqpayr          s                   like(estManPrem)
     d qqprem          s                   like(estManPrem)
     d qqtax           s                   like(estManPrem)
     d classPrem       s                   like(estManPrem)
     d mthSrPrem       s                   like(estManPrem)
     d mthTotPrem      s                   like(estManPrem)
     d terrPayroll     s                   like(SRAMTP)
     d sysDate         s                   like(SRDT)
     d sysTime         s                   like(SRTIME)
     d/copy sprdate

     c     bsky          klist
     c                   kfld                    ELCO#
     c                   kfld                    kkkey

     c     epky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELFYR
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    SRCLAS

     c     epky1         klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELFYR
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    SRCLAS
     c                   kfld                    SRSPEC
     c                   kfld                    SREXT
     c                   kfld                    kkRptDt

     c     f1ky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELFYR

     c     srky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c                   read      pqr01
     c                   dow       not(%eof)
     c                   delete    pqr01
     c                   read      pqr01
     c                   enddo

     c                   eval      sysTime = TimeTo6(SystemTime)
     c                   eval      sysDate = DateTo7(SystemDate)
     c                   clear                   svco#
     c                   clear                   svfnd
     c                   read      pel0l
     c                   dow       not(%eof)

     c                   select
     c                   when      ELANDT <> 0
     c                   when      ELEMD1 > ELPFDT
     c                   when      ELEMD2 > ELPFDT
     c                   when      ELEMD3 > ELPFDT
     c                   other
     c                   read      pel0l
     c                   iter
     c                   endsl

     c                   if           svco# <> ELCO#
     c                             or svfnd <> ELFND
     c                   eval      svco# = ELCO#
     c                   eval      svfnd = ELFND
     c     f1ky          chain     pf10l
     c                   if        not(%found)
     c     f1ky          setgt     pel0l
     c                   read      pel0l
     c                   iter
     c                   endif
     c                   endif

     c                   movel(p)  ELBSCD        kkkey
     c     bsky          chain     pbs01
     c                   if        not(%found) or
     c                             BSONLI <> 'Y'
     c                   read      pel0l
     c                   iter
     c                   endif

      * For each month in policy year . . .

     c                   movel(p)  ELPFDT        polExpMth
     c                   add       10000         polExpMth
     c                   do        13            x

     c                   eval      y = x-1
     c                   if        y = 0
     c                   movel(p)  ELPFDT        polRptMth
     c                   else
     c     *CYMD         move      ELPFDT        workDate
     c     workDate      adddur    y:*months     workDate
     c     *CYMD         move      workdate      cymdDate
     c                   movel(p)  cymdDate      polRptMth
     c                   endif

      * Calc EOM date for later use as a rpt date in EPKY1

     c                   movel     polRptMth     kkRptDt
     c                   move      31            kkRptDt

      * Leave if the calculated rpt mth exceeds the exp mth of the policy

     c                   if        polRptMth > polExpMth
     c                   leave
     c                   endif

      * Retrieve sum of S/R Billings from A/R for the month

     c                   clear                   mthARBilling
     c/exec sql
     c+ select sum(EAAMT)
     c+   into :mthARBilling
     c+   from WMEAP
     c+   where EACO#  = :ELCO#
     c+    and  EAFND  = :ELFND
     c+    and  EAEMP# = :ELEMP#
     c+    and  (EADIV = :ELDIV or EARDIV = :ELDIV)
     c+    and  EAFYR  = :ELFYR
     c+    and  EAARCD = :F1SRBB
     c+    and  dec(substr(char(EAPODT),1,5))  = :polRptMth
     c/end-exec

      * If no A/R Bills written, go to the next month.

     c                   if        mthARBilling = 0
     c                   iter
     c                   endif

     c                   clear                   terrPayroll
     c                   clear                   srManPrem
     c                   clear                   mthSRPrem

      * get Self-Reported info for the month

     c     srky          setll     psr0l5
     c     srky          reade     psr0l5
     c                   dow       not(%eof)

     c                   if        SRAMTP = 0
     c     srky          reade     psr0l5
     c                   iter
     c                   endif

     c                   select
     c                   when      SRTODT <> 0
     c                   movel(p)  SRTODT        srRptMth
     c                   when      SRRPMY <> 0
     c                   movel(p)  SRRPMY        srRptMth
     c                   endsl

     c                   if        srRptMth <> polRptMth
     c     srky          reade     psr0l5
     c                   iter
     c                   endif

      * get latest rate as of the report date

     c     epky1         setgt     pep0l1e
     c     epky          readpe    pep0l1e
     c                   dow       not(%eof) and EPSTAT <>'A'
     c     epky          readpe    pep0l1e
     c                   enddo

      * Retrieve the payrolls for each class for the month. Multiply each by
      * its rate to calc the manual premium.  Total the manual premium from each class.


     c                   if        srRptMth  = polRptMth
     c                   eval      terrPayroll = terrPayroll + SRAMTP
     c                   eval(h)   srManPrem = (srManPrem +
     c                                         (SRAMTP * EPARAT/100))
     c                   endif

     c     srky          reade     psr0l5
     c                   enddo


      * Call SRPREM to calculate total premium for the month.

     c                   eval      elrrn = dbrrn
     c                   eval      qqrpmy = dt6MDYY(polRptMth)
     c                   call      'SRPREM'
     c                   parm      elrrn         qqrrn
     c                   parm                    qqrpmy
     c                   parm                    qqtodt
     c                   parm      srManPrem     qqmapr
     c                   parm      0             qqadld
     c                   parm      terrPayroll   qqpayr
     c     mthTotPrem    parm      0             qqprem
     c                   parm      0             qqtax

      * This program is looking for payrolls that were zeroed out after
      * S/R Online wrote the A/R Billing, so a 10% difference between A/R
      * Billings and the calculated total premium for the month is an acceptable
      * tolerance.

     c                   if        %abs(1 - mthTotPrem/mthARBilling) > .1
     c                   exsr      wrtError
     c                   endif

     c                   enddo                                                  1 thru 13

     c                   read      pel0l
     c                   enddo

     c                   seton                                        lr
      *========================================================================
      * wrtError - write out of QRYWASRP outfile
      *========================================================================
     c     wrtError      begsr
     c                   eval      QRCO#  = ELCO#
     c                   eval      QRFND  = ELFND
     c                   eval      QREMP# = ELEMP#
     c                   eval      QRDIV  = ELDIV
     c                   eval      QRFYR  = ELFYR
     c                   eval      QRRPMY = polRptMth
     c                   eval      QRSRBL = mthTotPrem
     c                   eval      QRARBL = mthARBilling
     c                   eval      QRUSER = Q1USER
     c                   eval      QRPGM  = Q1PGM
     c                   eval      QRDATE = sysDate
     c                   eval      QRTIME = sysTime
     c                   write     pqr01
     c                   endsr
