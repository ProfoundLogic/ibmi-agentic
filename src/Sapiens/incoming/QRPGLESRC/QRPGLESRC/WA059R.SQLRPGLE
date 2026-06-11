     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *=============================================================================================
      /title WA059R - API to Transfer Audit Credits to Prem
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H bnddir('WTBNDDIR')
      *=============================================================================================
      *                                                                                            *
      *  This program is called from W7720R (Audit Bill). It runs through A/R for the policy       *
      *  and transfers any credits found in any category to premium. It then calls WA055R to       *
      *  dissolve the premium credit to any debit balances that exist.                             *
      *                                                                                            *
      *  Parameters:                                                                               *
      *    @@rrn - rrn for WDELP                                                                   *
      *                                                                                            *
      *  CALLED FROM: W7720R                                                                       *
      *                                                                                            *
      *  CREATED 10/27/05   C:136   PROJECT: WC1191                                                *
      *=============================================================================================
      * REVISIONS:                                                                                 *
      *                                                                                            *
      * 07/12/12 C:136 Support: WA146 80226                                                        *
      *                Correct installment fee balance calculation                                 *
      *                                                                                            *
     H* 11/07/11 C:190 Project: WC2022.
     H*                Recompiled.
     H*
      * 02/03/10 C:65  Project: WC1663/Supp#71465                                                  *
      *                Only process the credit transfers if the current year transfer flags are    *
      *                set to 'Y'.  Additionally, bail out on the estimated/revised transfer if    *
      *                the cash pay a/r codes are not set up.                                      *
      *                                                                                            *
      * 05/03/09 C:136 Project: WC1663                                                             *
      *                Add functionality to transfer all category credits to premium IAW the       *
      *                cash dissolve hierarchy when the policy is not audited. Also                *
      *                reenable the audit transfer for the deposit category.                       *
      *                                                                                            *
      * 01/12/09 C:136 Project: WC1769 Priss: 18                                                   *
      *                Add sixth and seventh assessments to this program.                          *
      *                                                                                            *
      * 04/25/07  C:136 Support: WA099 56590                                                       *
      *                 Disable deposit transfer. Deposits should not be transferred by audit      *
      *                 since deposit balance may accumulate across years.                         *
      *=============================================================================================
      * Group First Auxiliary File
     fwdf1l     if   e           k disk
      *** begin add *** 02/03/10 *************************************************
     fwdf2l     if   e           k disk
      *** end add ***** 02/03/10 *************************************************
      *
      * Employer Detail File
     fwdelp     if   e             disk    rename(pel01 : pel0p)
     fwdell     if   e           k disk    rename(pel01 : pel0l)
      *
      * A/R Batch Detail File
     fwdeap     if   e             disk    rename(pea01 : dea0p) prefix(d)
      *
      * A/R Billing File
     fwmeal6    uf a e           k disk    rename(pea01 : mea0l6) prefix(m6)
      *
      * Table File
     fwtabl     if   e           k disk
      *** beg add ***** 05/03/09 ******************************************************
      * Installment Fee File
     fwmifl     if   e           k disk    rename(pif01 : pif0l)
      * Cash Disbursement File  co/fnd/hier
     fwdcdl     if   e           k disk
      * A/R Premium Category Info
     fwtc3l     if   e           k disk
      * Rate Set Code File
     fwmrsl     if   e           k disk    rename(prs01 : prs0l)
      *** end add ***** 05/03/09 ******************************************************
    d*=============================================================================================
    d* Stand Alone Fields - TOP
    d*=============================================================================================
     d   kkco#         s                   like(DEACO#)
     d   kkfnd         s                   like(DEAFND)
     d   kkemp#        s                   like(DEAEMP#)
     d   kkdiv         s                   like(DEADIV)
     d   kkfyr         s                   like(DEAFYR)
     d   kkPodt        s                   like(DEAPODT)
     d   kkbscd        s                   like(BSKEY)
     d   payment       s                   like(DEAAMT)
     d   priorYrFlg    s               n   inz(*off)
     d   sysTime       s                   like(DEATIME)
     d   sysDate       s                   like(DEADT)
     d   newBusFlg     s                   like(DEASTAT) inz('N')
     d   ##call        s                   like(DEASTAT) inz('A')
      *** beg add ***** 01/12/09 *******************************************************
     d   #OfCtgy       c                   16
      *** end add ***** 01/12/09 *******************************************************
      *** beg add ***** 05/03/09 *******************************************************
     d  arCodes        s                   like (M6EAARCD) dim(99)
     d  catCodes       s                   like (M6EASTAT) dim(99)
     d  ctyTyp         s              2    dim(99) ascend
     d  catBalance     s             +2    like (M6EAAMT) dim(99)
     d  cashDslvErr    s                   like (M6EASTAT) inz(' ')
     d  auditFlag      s                   like (M6EASTAT) inz(' ')
     d  calcDeposit    s                   like (M6EASTAT) inz(' ')
     d  currYrDepMode  s                   like (M6EASTAT) inz(' ')
     d  ix1            s              2  0
     d  ix2            s              2  0
      *** end add ***** 05/03/09 *******************************************************
     d*** Begin Add *** 10/03/24 *******************************************
     d decInvoice#     s             10  0 Inz(0)
     d*** End   Add *** 10/03/24 *******************************************
    d*=============================================================================================
    d* Stand Alone Fields - Bottom
    d*=============================================================================================
    d*=============================================================================================
    d* Data Structures and Arrays
    d*=============================================================================================
     d psds          esds                  extname(szq1p)

     d
     d  xfrCdeDs       ds
     d F1AXDD
     d F1AXEC
     d F1AXGF
     d F1AXLF
     d F1AXMF
     d F1AXDP
     d F1AXSF
     d F1AXTX
     d F1AXTD
     d F1AX1T
     d F1AX2T
     d F1AX3T
     d F1AX4T
     d F1AX5T
      *** beg add ***** 01/12/09 *******************************************************
     d F1AX6T
     d F1AX7T
      *** end add ***** 01/12/09 *******************************************************
     d  xfrCdes                            overlay(xfrCdeDs)
      *** beg chg ***** 01/12/09 *******************************************************
     d**********                           like(M6EAARCD) dim(14)
     d                                     like(M6EAARCD) dim(#OfCtgy)
      *** end chg ***** 01/12/09 *******************************************************

     d xfrBalDs        ds
     d deducts                             like(M6EAAMT) inz(0)
     d expCons                             like(M6EAAMT) inz(0)
     d genFees                             like(M6EAAMT) inz(0)
     d latFees                             like(M6EAAMT) inz(0)
     d memFees                             like(M6EAAMT) inz(0)
     d deposit                             like(M6EAAMT) inz(0)
     d safFees                             like(M6EAAMT) inz(0)
     d stTaxes                             like(M6EAAMT) inz(0)
     d txOnDep                             like(M6EAAMT) inz(0)
     d firstAs                             like(M6EAAMT) inz(0)
     d secndAs                             like(M6EAAMT) inz(0)
     d thirdAs                             like(M6EAAMT) inz(0)
     d forthAs                             like(M6EAAMT) inz(0)
     d fifthAs                             like(M6EAAMT) inz(0)
      *** beg add ***** 01/12/09 *******************************************************
     d sixthAs                             like(M6EAAMT) inz(0)
     d sevenAs                             like(M6EAAMT) inz(0)
      *** end add ***** 01/12/09 *******************************************************
     d  xfrBals                            overlay(xfrBalDs)
      *** beg chg ***** 01/12/09 *******************************************************
     d***********                          like(M6EAAMT) dim(14)
     d                                     like(M6EAAMT) dim(#OfCtgy)
      *** end chg ***** 01/12/09 *******************************************************
    d*=============================================================================================
    d* Procedure Prototypes
    d*=============================================================================================
     d*entry
     d WA059R          pr
     d  @@rrn                         9  0                                      of WDELP
     d WA059R          pi
     d  @@rrn                         9  0

     d GetPInfo        pr
     d   rrn                          9  0 Const                                of WDELP

     d GetTrBals       pr

     d PostXPays       pr

     d WriteAR         pr
     d   payment                           const like(M6EAAMT)                    of WDELP
     d   arCode                            const like(M6EAARCD)

     d ErrChk          pr             1

      *** beg chg ***** 05/03/09 ***********************************************
     D***********CashDissolve     PR                  extpgm('WA055R')
     D*********** ##call                         1    const
     D*********** elrrn                          9  0
     DCashDissolve     PR                  extpgm('WA055R')
     D ##call                         1    const
     D elrrn                          9  0 const
      *** end chg ***** 05/03/09 ***********************************************

     d fixTime         pr

     d/copy wprar
     d/copy sprdate
      *** beg add ***** 05/03/09 ***********************************************
     d GetARCodes      pr
     d   cdArray                           like(M6EAARCD) dim(99)
     d   catArray                          like(M6EASTAT) dim(99)
     d   error                             like(M6EASTAT)
     d GetCatBalance   pr
     d GetType         pr             1
     d   arType                       1    const
     d   arCat                        1    const
     d CvtToPrem       pr
     d GetDepBalance   pr
     d   idx1                         2  0
     d   idx2                         2  0
      *** end add ***** 05/03/09 ***********************************************
    c*=============================================================================================
      *  MAINLINE PROCESSING
    c*=============================================================================================
     c                   callp     GetPInfo(@@rrn)
      *** beg add ***** 05/03/09 *************************************************
     c                   if        auditFlag = 'Y'
      *** end add ***** 05/03/09 *************************************************
      *** begin add *** 02/03/10 *************************************************
     c*
     c* Only process if flagged to dissolve credits for Audits.
     c                   if        F2ACYF <> 'Y'
     c                   seton                                        lr
     c                   return
     c                   endif
      *** end add ***** 02/03/10 *************************************************
     c                   eval      *inlr = ErrChk
     c                   if        *inlr = *on
     c                   return
     c                   endif
     c                   callp     GetTrBals
     c                   callp     PostXPays
     c                   callp     CashDissolve(##call : @@rrn)
     c                   callp     fixTime
      *** beg add ***** 05/03/09 *************************************************
     c                   else
      *** begin add *** 02/03/10 *************************************************
     c*
     c* Only process if flagged to dissolve credits for Estimated/Revised.
     c                   if        F2RCYF <> 'Y'
     c                   seton                                        lr
     c                   return
     c                   endif
      *** end add ***** 02/03/10 *************************************************
     c                   callp     getARCodes(arCodes:
     c                                        catCodes:
     c                                        cashDslvErr)
     c                   if        cashDslvErr <> *blank
     c                   seton                                        lr
     c                   return
     c                   endif
     c                   callp     GetCatBalance
     c                   callp     CvtToPrem
     c                   callp     CashDissolve('E' : @@rrn)
     c                   callp     fixTime
     c                   endif
      *** end add ***** 05/03/09 *************************************************
     c                   seton                                        lr
     p*===========================================================================
     p* GetPInfo - Gets key values from policy information
     p*===========================================================================
     p GetPInfo        b                   export
     d GetPInfo        pi
     d   rrn                          9  0 const

     c     f1ky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
      *** begin add *** 02/03/10 *************************************************
     c     f2ky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
      *** end add ***** 02/03/10 *************************************************

     c     rrn           chain     pel0p
     c                   eval      kkco#  = ELCO#
     c                   eval      kkfnd  = ELFND
     c                   eval      kkemp# = ELEMP#
     c                   eval      kkdiv  = ELDIV
     c                   eval      kkfyr  = ELFYR
      *** beg add ***** 05/03/09 ***********************************************
     c                   if        ELFAF = 'A'
     c                   eval      auditFlag = 'Y'
     c                   else
     c                   eval      auditFlag = *blank
     c                   endif
      *** end add ***** 05/03/09 ***********************************************

     c     f1ky          chain     pf101
      *** beg add ***** 05/03/09 ***********************************************
     c                   eval      currYrDepMode = F1DPYM
      *
      * Get installment fee payment code
      *
     c     rsky          klist
     c                   kfld                    kkco#
     c                   kfld                    ELRTCD

     c     ifky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    RSJRST

     c     ifky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    RSJRST
     c                   kfld                    ELPFDT

     c     rsky          chain     prs0l
     c     ifky1         setgt     pif0l
     c     ifky          readpe    pif0l
      *** end add ***** 05/03/09 ***********************************************
      *** begin add *** 02/03/10 *************************************************
     c     f2ky          chain     pf201
      *** end add ***** 02/03/10 *************************************************

     p GetPInfo        e
     p*===========================================================================
     p* GetTrBals - Gets A/R balance for all audit transfer codes
     p*===========================================================================
     p GetTrBals       b                   export
     d GetTrBals       pi

     d   x             s              3  0 inz(1)

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr

     c                   clear                   xfrBals
     c     eaky          setll     mea0l6
     c     eaky          reade(n)  mea0l6
     c                   dow       not(%eof)

     c                   select
     c                   when      GetArCat(M6EAARCD) = 'R'
     c                   eval      deducts = deducts + ArValue(M6EAAMT:M6EAARTY)
     c                   when      GetArCat(M6EAARCD) = 'E'
     c                   eval      expCons = expCons + ArValue(M6EAAMT:M6EAARTY)
     c                   when      GetArCat(M6EAARCD) = 'F'
     c                   eval      genFees = GenFees + ArValue(M6EAAMT:M6EAARTY)
     c                   when      GetArCat(M6EAARCD) = 'L'
     c                   eval      latFees = latFees + ArValue(M6EAAMT:M6EAARTY)
     c                   when      GetArCat(M6EAARCD) = 'M'
     c                   eval      memFees = memFees + ArValue(M6EAAMT:M6EAARTY)
      *** beg del ***** 04/25/07 *******************************************************
     c***********        when      GetArCat(M6EAARCD) = 'D' and
     c***********                  (M6EAARTY = 'H' or M6EAARTY = 'I')
     c***********        eval      deposit = deposit + ArValue(M6EAAMT:M6EAARTY)
      *** end del ***** 04/25/07 *******************************************************
     c                   when      GetArCat(M6EAARCD) = 'S'
     c                   eval      safFees = safFees + ArValue(M6EAAMT:M6EAARTY)
     c                   when      GetArCat(M6EAARCD) = 'T'
     c                   eval      stTaxes = stTaxes + ArValue(M6EAAMT:M6EAARTY)
      *** beg del ***** 04/25/07 *******************************************************
     c***********        when      GetArCat(M6EAARCD) = 'D' AND
     c***********                  (M6EAARTY = 'F' or M6EAARTY = 'Q')
     c***********        eval      txOnDep = txOnDep + ArValue(M6EAAMT:M6EAARTY)
      *** end del ***** 04/25/07 *******************************************************
     c                   when      GetArCat(M6EAARCD) = '1'
     c                   eval      firstAs = firstAs + ArValue(M6EAAMT:M6EAARTY)
     c                   when      GetArCat(M6EAARCD) = '2'
     c                   eval      secndAs = secndAs + ArValue(M6EAAMT:M6EAARTY)
     c                   when      GetArCat(M6EAARCD) = '3'
     c                   eval      thirdAs = thirdAs + ArValue(M6EAAMT:M6EAARTY)
     c                   when      GetArCat(M6EAARCD) = '4'
     c                   eval      forthAs = forthAs + ArValue(M6EAAMT:M6EAARTY)
     c                   when      GetArCat(M6EAARCD) = '5'
     c                   eval      fifthAs = fifthAs + ArValue(M6EAAMT:M6EAARTY)
      *** beg add ***** 01/12/09 *******************************************************
     c                   when      GetArCat(M6EAARCD) = '6'
     c                   eval      sixthAs = sixthAs + ArValue(M6EAAMT:M6EAARTY)
     c                   when      GetArCat(M6EAARCD) = '7'
     c                   eval      sevenAs = sevenAs + ArValue(M6EAAMT:M6EAARTY)
      *** end add ***** 01/12/09 *******************************************************
     c                   endsl

     c     eaky          reade(n)  mea0l6
     c                   enddo

      *** beg add ***** 05/03/09 *******************************************************
     c                   if        F1AXDP <> *blanks  or
     c                             F1AXtd <> *blanks
     c                   eval      ix1 = 1
     c                   eval      ix2 = 2
     c                   callp     GetDepBalance(ix1 : ix2)
     c                   eval      deposit = catBalance(ix1)
     c                   eval      txOnDep = catBalance(ix2)
     c                   endif
      *** end add ***** 05/03/09 *******************************************************

     p GetTrBals       e
     p*===========================================================================
     p* PostXPays - Posts transfer payments
     p*===========================================================================
     p PostXPays       b                   export
     d PostXPays       pi

     d   x             s              3  0 inz(1)
     d   premXfer      s                   like(M6EAAMT)

     c                   eval      sysDate = DateTo7(SystemDate)
     c                   eval      sysTime = TimeTo6(SystemTime)

      *** beg chg ***** 01/12/09 **********************************************
     c*****1*****        do        14            x
     c     1             do        #OfCtgy       x
      *** end chg ***** 01/12/09 **********************************************
     c                   select

     c                   when      xfrBals(x) > 0
     c                   eval      xfrBals(x) = 0

     c                   when      xfrBals(x) < 0
     c                   if        xfrCdes(x) <> *blank
     c                   callp     writeAR((xfrBals(x)) : xfrCdes(x))
     c                   else
     c                   eval      xfrBals(x) = 0
     c                   endif

     c                   endsl
     c                   enddo

     c                   xfoot     xfrBals       premXfer
     c                   if        premXfer < 0
     c                   callp     writeAR((-premXfer) : F1AXPT)
     c                   endif

     p PostXPays       e
     p*===========================================================================
     p* WriteAR - Writes A/R
     p*===========================================================================
     p WriteAR         b                   export
     d WriteAR         pi
     d   payment                           const like(M6EAAMT)
     d   arCode                            const like(M6EAARCD)
     c                   clear                   mea0l6
     c                   eval      M6EACO#  = kkco#
     c                   eval      M6EAFND  = kkfnd
     c                   eval      M6EAEMP# = kkemp#
     c                   eval      M6EADIV  = kkdiv
     c                   eval      M6EARDIV = kkdiv
     c                   eval      M6EAFYR  = kkfyr
     c                   eval      M6EAAMT  = payment
     c                   eval      M6EAARCD = arCode
     c                   eval      M6EACTYP = GetARcat(M6EAARCD)
     c                   eval      M6EAARTY = GetArType(M6EAARCD)
     c                   eval      M6EADESC = GetArDesc(M6EAARCD)
     c                   eval      M6EATRDT = sysDate
     c                   eval      M6EAPODT = sysDate
     c*** Begin Add *** 10/03/24 *******************************************
                         M6EAnInv = GetInvoice#(kkCo#:kkFnd:kkEmp#:kkDiv:
                                                kkFyr:sysDate);
     c*** End   Add *** 10/03/24 *******************************************
     c                   eval      M6EAPGM  = Q1PGM
     c                   eval      M6EAUSER = Q1USER
     c                   eval      M6EATIME = sysTime
     c                   eval      M6EADT   = sysDate
     c                   write     mea0l6

     p writeAR         e
     p*===========================================================================
     p* ErrChk - returns *on if there are audit trnsfr code/s w/o a audit prem trsnfr code
     p*===========================================================================
     p ErrChk          b                   export
     d ErrChk          pi             1

     d   x             s              3  0 inz(1)
     d   liveOne       s               n   inz(*off)

      *** begin add *** 02/03/10 *************************************************
     c     cdky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
      *** end add ***** 02/03/10 *************************************************

      *** beg chg ***** 01/12/09 **********************************************
     c*****1*****        do        14            x
     c     1             do        #OfCtgy       x
      *** end chg ***** 01/12/09 **********************************************

     c                   if        xfrCdes(x) <> *blanks
     c                   eval      liveOne = *on
     c                   leave
     c                   endif

     c                   enddo

     c                   if            liveOne = *on
     c                             and F1AXPT <> *blanks
     c                   eval      liveOne = *off
     c                   endif
      *** begin add *** 02/03/10 *************************************************
     c*
     c* If audit current year flag is set to 'Y' then bail if no audit cash pay code.
     c                   if        F2ACYF = 'Y'
     c
     c     cdky          chain     pc301
     c
     c                   if        c3apcd = *blanks
     c                   eval      liveOne = *on
     c                   endif
     c
     c                   endif
      *** end add ***** 02/03/10 *************************************************

     c                   return    liveOne

     p ErrChk          e
     p*===========================================================================
     p* fixTime - ensure that podt/dt/time values are unique for A/R Inquiry
     p*===========================================================================
     p fixTime         b                   export
     d fixTime         pi

     d count           s                   like(M6EATIME)
     d svpodt          s                   like(M6EAPODT)
     d svdt            s                   like(M6EAPODT)
     d svtime          s                   like(M6EATIME)
     d kkpodt          s                   like(M6EAPODT)

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr

     c     eaky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr
     c                   kfld                    kkpodt

     c                   eval      count = 0
     c                   eval      kkpodt = *loval
     c     eaky1         setll     mea0l6
     c     eaky          reade     mea0l6
     c                   dow       not(%eof)

     c                   if        count <> 0

     c                   if            M6EAPODT = svpodt
     c                             and M6EADT   = svdt
     c                             and M6EATIME = svtime
     c                   eval      M6EATIME = M6EATIME + count
     c                   eval      count = count + 1
     c                   else
     c                   eval      svpodt = M6EAPODT
     c                   eval      svdt   = M6EADT
     c                   eval      svtime = M6EATIME
     c                   eval      count = 1
     c                   endif

     c                   else
     c                   eval      svpodt = M6EAPODT
     c                   eval      svdt   = M6EADT
     c                   eval      svtime = M6EATIME
     c                   eval      count = 1

     c                   endif

     c                   update    mea0l6
     c     eaky          reade     mea0l6
     c                   enddo

     p fixTime         e
      *** beg add ***** 05/03/09 **********************************************
     p*===========================================================================
     p* GetARCodes - Gets Cash  or NB codes from WDCDP and WTC3P and puts into array
     p*===========================================================================
     p GetARCodes      b                   export
     d GetARCodes      pi
     d   codeArr                           like (M6EAARCD) dim(99)
     d   catArr                            like (M6EASTAT) dim(99)
     d   error                             like (M6EASTAT)

     d   kkhier        s              5  0 inz(0)
     d   ctr           s              3  0 inz(1)
     d   x             s              3  0 inz(1)
     d   premSetUpFlg  s              1    inz('N')

     c     cdky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd

     c     cdky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkhier

     c                   clear                   codeArr
     c                   clear                   catArr
     c                   eval      error = '1'
     c                   z-add     1             ctr
     c     cdky1         setll     pcd01
     c     cdky          reade     pcd01
     c                   dow       not(%eof)
     c                   if        CDHIER < 10000
     c                   if        CDCTYP <> 'P'
     c                   eval      error = ' '
      *** begin add *** 02/03/10 *************************************************
     c                   if        CDCPCD = *blanks
     c                   eval      error = '1'
     c                   return
     c                   endif
      *** end add ***** 02/03/10 *************************************************
     c                   eval      codeArr(ctr) = CDNBCD
     c                   eval      catArr(ctr)  = CDCTYP
     c                   eval      ctyTyp(ctr) =
     c                             catArr(ctr) + getARType(codeArr(ctr))
     c                   if           codeArr(ctr) = IFIFPD
     c                             or codeArr(ctr) = IFIFTR
     c                             or codeArr(ctr) = IFIFTC
     c                   movel     'I'           ctyTyp(ctr)
     c                   endif
     c                   add       1             ctr
     c                   endif                                                  CDCTYP <> 'P'
     c                   if        CDCTYP = 'P'
     c                   eval      premSetUpFlg = 'Y'
     c     cdky          chain     pc301
     c                   if        C3CNCD = *blank
      *** begin add *** 02/03/10 *************************************************
     c                             or C3CPCD = *blanks
      *** end add ***** 02/03/10 *************************************************
     c                   eval      error = '2'
     c                   return
     c                   endif
     c                   eval      codeArr(ctr) = C3CNCD
     c                   eval      catArr(ctr)  = C3CTYP
     c                   eval      ctyTyp(ctr) =
     c                             catArr(ctr) + getARType(codeArr(ctr))
     c                   add       1             ctr
     c                   endif                                                  CDCTYP = 'P'
     c                   endif                                                  CDHIER < 10000
     c     cdky          reade     pcd01
     c                   enddo
     c                   if        premSetUpFlg = 'N'
     c                   eval      error = '3'
     c                   endif
     c                   return
     p getArCodes      e
     p*===========================================================================
     p* GetCatBalance - Get Category Balance for each Cash Dissolve Hierarchy Element
     p*===========================================================================
     p getCatBalance   b
     d getCatBalance   pi

     d idx             s              2  0 inz(1)
     d idx1            s              2  0 inz(1)
     d idx2            s              2  0 inz(1)
     d xxCtyTyp        s              2
     d arCat           s              1
     d arType          s              1

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr

     c                   clear                   catBalance
     c     eaky          setll     mea0l6
     c     eaky          reade(n)  mea0l6
     c                   dow       not(%eof)
     c                   eval      arCat  = getARCat(M6EAARCD)
     c                   if        arCat <> 'P' and arCat <> 'D'
     c                   eval      arType = getARType(M6EAARCD)
     c                   eval      xxCtyTyp = arCat +
     c                             GetType(arType : arCat)
     c                   if           M6EAARCD = IFIFPD
     c                             or M6EAARCD = IFIFTR
     c                             or M6EAARCD = IFIFTC
      *** beg add ***** 07/12/12 *******************************************************
     c                             or M6EAARCD = IFIFBL
      *** end add ***** 07/12/12 *******************************************************
     c                   movel     'I'           xxCtyTyp
     c                   endif                                                  M6EAARCD
     c                   eval      idx = 1
     c     xxCtyTyp      lookup    ctyTyp(idx)                            60
     c                   if        %equal
     c                   eval      catBalance(idx) =
     c                             catBalance(idx) + ArValue(M6EAAMT:M6EAARTY)
     c                   endif                                                  %equal
     c                   endif                                                  arCat <> 'P'
     c     eaky          reade(n)  mea0l6
     c                   enddo
      *
      * Find index for deposit and deposit tax and get the balances if they exist
      * in the hierarchy
      *
     c     'DI'          lookup    ctyTyp(idx1)                           60
     c                   if        not(%found)
     c                   eval      idx1 = 0
     c                   endif
     c     'DQ'          lookup    ctyTyp(idx2)                           60
     c                   if        not(%found)
     c                   eval      idx2 = 0
     c                   endif
     c                   if        idx1 <> 0 or idx2 <> 0
     c                   callp     GetDepBalance(idx1 : idx2)
     c                   endif

     p getCatBalance   e
     p*===========================================================================
     p* getType - gets hierarchy type for lookup
     p*===========================================================================
     p getType         b
     d getType         pi             1
     d   arType                       1    const
     d   arCat                        1    const

     c                   select
     c                   when        arCat = 'E'  or
     c                               arCat = 'P'  and
     c                             (arType = 'N'  or
     c                              arType = 'R'  or
     c                              arType = 'V'  or
     c                              arType = 'T'  or
     c                              arType = 'R'  or
     c                              arType = 'P')
     c                   return    'P'
     c                   when        arCat = 'D'  and
     c                             (arType = 'I'  or
     c                              arType = 'R'  or
     c                              arType = 'T') or
     c                               arType = 'H'
     c                   return    'I'
     c                   when      arType = 'A' or
     c                             arType = 'B' or
     c                             arType = 'D'
     c                   return    'P'
     c                   when      arType = 'F' or
     c                             arType = 'Q'
     c                   return    'Q'
     c                   other
     c                   return    *blank
     c                   endsl

     p getType         e
     p*===========================================================================
     p* CvtToPrem - loops through CatBalance array, posts offsets for each category
     p*             with a credit balance and then writes a premium credit for the
     p*             entire credit amount
     p*===========================================================================
     p CvtToPrem       b
     d CvtToPrem       pi

     d   x             s              3  0 inz(1)
     d   premXfer      s             +2    like(M6EAAMT)

     c                   eval      sysDate = DateTo7(SystemDate)
     c                   eval      sysTime = TimeTo6(SystemTime)

     c     1             do        99            x
     c                   select

     c                   when      catBalance(x) > 0
     c                   eval      catBalance(x) = 0

     c                   when      catBalance(x) < 0
     c                   if        arCodes(x)    <> *blank
     c                   callp     writeAR((catBalance(x)) : arCodes(x))
     c                   else
     c                   eval      catBalance(x) = 0
     c                   endif

     c                   endsl
     c                   enddo

     c                   xfoot     catBalance    premXfer
     c                   if        premXfer < 0
      *** beg chg ***** 02/03/10 ***********************************************
     c***********        callp     writeAR((-premXfer) : C3CPCD)
     c                   callp     writeAR((-premXfer) : C3CNCD)
      *** end chg ***** 02/03/10 ***********************************************
     c                   endif

     p CvtToPrem       e
     p*===========================================================================
     p* getDepBalance - loads the catBalance array with the deposit balance or deposit
     p*                 tax balance in the index position passed in the subprocedure
     p*===========================================================================
     p getDepBalance   b
     d getDepBalance   pi
     d  idx1                          2  0                                      deposit
     d  idx2                          2  0                                      deposit tax

     d prYrDepMode     s              1    inz(' ')
     d arCat           s              1
     d svFyr           s                   like(M6EAFYR)

     c     eaky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv

     c     eaky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr

     c     f1ky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    M6EAFYR

     c     f1kysv        klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr

     c                   select
     c                   when      currYrDepMode = 'Y'
     c     eaky1         setll     mea0l6
     c     eaky1         reade(n)  mea0l6
     c                   dow       not(%eof)
     c                   if        getARCat(M6EAARCD) = 'D'
     c                   select
     c                   when      idx2 <> 0 and
     c                             (M6EAARTY = 'F' or
     c                              M6EAARTY = 'Q')
     c                   eval      catBalance(idx2) =
     c                             catBalance(idx2) + ArValue(M6EAAMT:M6EAARTY)
     c                   when      idx1 <> 0
     c                   eval      catBalance(idx1) =
     c                             catBalance(idx1) + ArValue(M6EAAMT:M6EAARTY)
     c                   endsl
     c                   endif
     c     eaky1         reade(n)  mea0l6
     c                   enddo
     c                   when      currYrDepMode <> 'Y'
     c     f1ky          chain     pf101
     c                   eval      prYrDepMode = F1DPYM
     c                   eval      svFyr = M6EAFYR
     c     eaky          setll     mea0l6
     c     eaky          reade(n)  mea0l6
     c                   dow       not(%eof) and M6EAFYR <= kkfyr
     c                   if        getARCat(M6EAARCD) = 'D'
     c                   if        svFyr <> M6EAFYR
     c     f1ky          chain     pf101
     c                   eval      prYrDepMode = F1DPYM
     c                   endif
     c                   if        prYrDepMode <> 'Y'
     c                   select
     c                   when      idx2 <> 0 and
     c                             (M6EAARTY = 'F' or
     c                              M6EAARTY = 'Q')
     c                   eval      catBalance(idx2) =
     c                             catBalance(idx2) + ArValue(M6EAAMT:M6EAARTY)
     c                   when      idx1 <> 0
     c                   eval      catBalance(idx1) =
     c                             catBalance(idx1) + ArValue(M6EAAMT:M6EAARTY)
     c                   endsl
     c                   endif
     c                   endif
     c     eaky          reade(n)  mea0l6
     c                   enddo
     c     f1kysv        chain     pf101
     c                   endsl
     p getDepBalance   e
      *** end add ***** 05/03/09 **********************************************
     p*** Begin Add *** 10/03/24 *******************************************
     p*===========================================================================
     p* GetInvoice# - Assign an Invoice #.                                  or deposit
     p*===========================================================================
     p GetInvoice#     b
     d GetInvoice#     pi            10  0
     d  inCompany#                    3  0                                      deposit
     d  inGroup#                      3  0                                      deposit tax
     d  inEmployer#                   9  0                                      deposit tax
     d  inDivision                    5  0                                      deposit tax
     d  inYear                        3  0                                      deposit tax
     d  inPostDate                    7  0                                      deposit tax

     d inKey           s             20a   Inz(' ')

       decInvoice# = 0;
       Exec Sql Select EAninv
                Into :decInvoice#
                From WMEAP
                Where EAco#=:inCompany# And EAfnd=:inGroup# And
                EAemp#=:inEmployer# And EAdiv=:inDivision And EAfyr=:inYear
                And EACTyp<> 'C' And EACTyp <> 'R' And
                EApoDt=:inPostDate And EAarTy in ('A','B','D','F','H');

       If decInvoice# = 0;
          inKey = %EditC(inCompany#:'X') + %EditC(inGroup#:'X') +
                  %EditC(inEmployer#:'X') + %EditC(inDivision:'X');
          decInvoice# = GetNext3rdPartySeq#(inKey);
       EndIf;

       Return decInvoice#;

     p GetInvoice#     e
     p*** End   Add *** 10/03/24 *******************************************
