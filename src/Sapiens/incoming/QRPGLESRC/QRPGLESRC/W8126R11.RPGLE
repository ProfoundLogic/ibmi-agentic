     * (C) Copyright Progressive Data Solutions, Inc., 2004
      /title W8126R11 - Clarendon - add Self-Reporting Accounts to Aged Receivables
     h option(*nodebugio: *srcstmt)
     h debug(*yes)
     H dftactgrp(*no) actgrp(*caller)
     h datfmt(*usa)
     *=============================================================================================
     * Description:  Drive through INFCP. Pick up the self-reporting policies, check if policy    *
     *               was picked up in AGEDP. If not, calculate written premium, subtract cash     *
     *               payments and write a record to AGEDP reporting the difference into the       *
     *               deferred bucket.                                                             *
     *                                                                                            *
     * Created by C:136 09/28/04 for Support Seq#: 43967
     *                                                                                            *
     *    Called from W8126C                                                                      *
     *=============================================================================================
      * Revised:                                                                                   *
      *                                                                                            *
      * 11/04/04 C:136  Support: 49637                                                             *
      *                 Include R,N,V,T transactions in calculation of deferred.                   *
      *                                                                                            *
     *=============================================================================================
     finfcp     if   e             disk
     fagedl     if   e           k disk    rename(age01:age00)
     fwfeul     if   e           k disk    rename(peu01:peu00)
     fwfeul3    if   e           k disk    rename(peu01:peu03)
     fwdell     if   e           k disk
     fwtabl     if   e           k disk
     fwmupl     if   e           k disk
     Fwmeal4    if   e           k disk    rename(pea01:pea04)
     fagedp     o  a e             disk    rename(age01:age0p)
    d*
     d psds          esds                  extname(szq1p)
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
     d kkkey           s                   like(TTKEY)
     d svdiv           s                   like(EUDIV)
     d expConstant     s                   like(ELPRTD)
     d srWrtnPrem      s             20  2
     d srRecvdAmt      s             20  2
     d srDeferred      s             20  2
     *---------------------------------------------------------------------
     * Stand Alone Fields - BOTTOM
     *---------------------------------------------------------------------
     *
     *  AGEDP  - Clarendon Aging
     *
     c     agky          klist
     c                   kfld                    FCFND
     c                   kfld                    FCEMP#
     c                   kfld                    FCFYR
     c                   kfld                    FCRST
     *
     *  WFEUL  - Calendar Report
     *
     c     euky          klist
     c                   kfld                    UPCO#
     c                   kfld                    FCFND
     c                   kfld                    FCEMP#
     c                   kfld                    FCFYR
     c                   kfld                    FCRST
     *
     c     euky3         klist
     c                   kfld                    UPCO#
     c                   kfld                    FCFND
     c                   kfld                    FCRST
     c                   kfld                    FCFYR
     c                   kfld                    FCEMP#
     *
     *  WDELL  - Employer Detail
     *
     c     elky          klist
     c                   kfld                    UPCO#
     c                   kfld                    EUFND
     c                   kfld                    EUEMP#
     c                   kfld                    EUDIV
     c                   kfld                    EUFYR
     *
     *  WTBSP  - Billing Schedule Code
     *
     c     bsky          klist
     c                   kfld                    UPCO#
     c                   kfld                    kkkey
     *
     *  WTTTP  - Transaction Type Table
     *
     c     ttky          klist
     c                   kfld                    UPCO#
     c                   kfld                    kkkey
      *
      *  WMEAP -  A/R in PDSMTH
      *
     c     eaky          klist
     c                   kfld                    UPCO#
     c                   kfld                    EUFND
     c                   kfld                    EUEMP#
     c                   kfld                    EUDIV
     c                   kfld                    EUFYR
     *
     *  WMUPP - User Parms
     *
     c     upky          klist
     c                   kfld                    Q1USER
     *
     *
      * Get user co# for keys
      *
    c     upky          chain     pup01
      *
     * Drive through INFCP
     *
     c                   read      INFCP
     c                   dow       not(%eof)
      *
      * Check if policy is already included in AGEDP
      *
     c     agky          chain     age00
     c                   if        not(%found)
      *
      * Find out if division is self-reporting
      *
     c     euky          setgt     peu00
     c     euky          readpe    peu00
     c                   if        not(%eof)
      *
     c     elky          chain     pel01
     c                   if        %found and ELFAF <> 'A'
      *
     c                   movel(p)  ELBSCD        kkkey
     c     bsky          chain     pbs01
     c                   if        %found and BSSEFL = 'S'
      *
      * Get expense constant, there is only one charge per policy.
      * Get written premium from WFEUP
      * Get cash received on division break
      * Add expense constant
      *
     c                   eval      svdiv = -1
     c                   clear                   expConstant
     c                   clear                   srWrtnPrem
     c                   clear                   srRecvdAmt
     c                   clear                   srDeferred
     c     euky3         setll     peu03
     c     euky3         reade     peu03
     c                   dow       not %eof
      *
      * Get expense constant
      *
     c                   if        expConstant = 0
     c     elky          chain     pel01
     c                   add       ELPRTD        expConstant
     c                   endif                                                  expConsant = 0
      *
     c                   add       EUPYWR        srWrtnPrem
     c                   add       EUYDWR        srWrtnPrem
      *
      * Get cash received on division break
      *
     c                   if        EUDIV <> svdiv
      *
     c     eaky          setll     pea04
     c     eaky          reade     pea04
     c                   dow       not %eof

     c                   eval      kkkey = EAARCD
     c     ttky          chain     ptt01
      *** beg chg ***** 11/04/04 **********************************************
     c*****************  if        %found and TTTYPE = 'P'
     c*****************  add       EAAMT         srRecvdAmt
     c*****************  endif                                                  %found

     c                   if        %found
     c
     c                   if        (TTTYPE = 'P' or
     c                              TTTYPE = 'T')
     c                   add       EAAMT         srRecvdAmt
     c                   endif
     c
     c                   if        (TTTYPE = 'N' or
     c                              TTTYPE = 'R' or
     c                              TTTYPE = 'V')
     c                   sub       EAAMT         srRecvdAmt
     c                   endif
     c
     c                   endif
      *** end chg ***** 11/04/04 **********************************************

     c     eaky          reade     pea04
     c                   enddo

     c                   eval      svdiv = EUDIV
     c                   endif                                                  EUDIV <> svdiv
      *
     c     euky3         reade     peu03
     c                   enddo
      *
      * Add the expense constant to premium now.
      *
     c                   add       expConstant   srWrtnPrem
      *
      * Since record wasn't included in AGEDP, the difference between premium and cash is
      * all deferred.
      *
     c                   eval      srDeferred = srWrtnPrem - srRecvdAmt
     c                   if        srDeferred <> 0
     c                   exsr      wrtAGEDP
     c                   endif
      *
     c                   endif                                                  %found & BSSEFL
      *
     c                   endif                                                  %found
      *
     c                   endif                                                  not(%eof)
      *
     c                   endif                                                  not(%found)
      *
     c                   read      INFCP
     c                   enddo
      *
     c                   eval      *inlr = *on
      *=============================================================================================
      * wrtAGEDP - write AGEDP
      *=============================================================================================
     c     wrtAGEDP      begsr
      *
     c                   clear                   age0p
      *
     c                   if        srDeferred > 0
     c                   eval      GEDEFF  = %editc(srDeferred:'P')
     c                   eval      GEDEFF# = srDeferred
     c                   move      '.00'         GEADPR
     c                   move      '.00'         GECURR
     c                   move      '.00'         GEAM00
     c                   else
     c                   eval      GEADPR  = %editc(srDeferred:'P')
     c                   eval      GEADPR# = srDeferred
     c                   eval      GECURR  = %editc(srDeferred:'P')
     c                   eval      GECURR# = srDeferred
     c                   eval      GEAM00  = %editc(srDeferred:'P')
     c                   eval      GEAM00# = srDeferred
     c                   move      '.00'         GEDEFF
     c                   endif
      *
     c                   eval      GEAMTD  = %editc(srDeferred:'P')
     c                   eval      GEPGID  = FCPGID
     c                   eval      GEPOL   = FCPOL
     c                   move      '.00'         GEAM30
     c                   move      '.00'         GEAM60
     c                   move      '.00'         GEAM90
     c                   eval      GEEOMD  = FCEOMD
     c                   eval      GEEFDT  = FCEFDT
     c                   eval      GERST   = FCRST
     c                   eval      GEFND   = FCFND
     c                   eval      GEFYR   = FCFYR
     c                   eval      GEEMP#  = FCEMP#
     c                   eval      GEAMTD# = srDeferred
      *
     c                   write     age0p
      *
     c                   endsr
