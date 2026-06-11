     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*****************************************************************
    H/TITLE W1302R - Upload Payment Transaction Information
    H*****************************************************************
    H*  PROGRAM DESCRIPTION:                                         *
    H*                                                               *
    H*  THIS PROGRAM WILL READ THE LOCKBOX UPLOAD FILE WXLUP AND     *
    H*  CREATE A/R BATCH PAYMENT RECORDS FROM THE DATA.              *
    H*                                                               *
    H*  CALLED BY:                                                   *
    H*   W1300C                                                      *
    H*                                                               *
    H*  CALLS TO:  NONE                                              *
    H*                                                               *
    H*  PROGRAMMER: 65                                               *
    H*  CREATED:    06/05/03          PROJECT: 129WAD                *
    H*****************************************************************
    H* REVISIONS:                                                    *
     H*                                                               *                            *
     H* 06/24/24 C:079 Project: 146WKR                                *
     H*                Collateral Processing - Recomile               *
     H*                                                               *
     H* 01/06/15 C:136 Support: PHW39177                              *
     H*                Fix Batch Total logic                          *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
    H*                                                               *
    H* 11/23/14 C:136 Support: WA129 87469                           *
    H*                Rewrite logic (no comments - cleaned up code)  *
    H*                                                               *
    H* 09/11/14 C:136 Support: WA129 86114                           *
    H*                Just run transaction one time if cash dissolve *
    H*                is invoked.                                    *
    H*                                                               *
    H* 05/22/14 C:136 Project: 129WSB (CoveRisk Lockbox)             *
    H*                Retrofit modified Lockbox process into Base    *
    H*                                                               *
     H* 07/18/11  C:190 Project: 71WLB                                *
    H*                 Receive the new dissolve flag parameter and   *
    H*                  use it to determine how to post the trans-   *
    H*                  -actions.                                    *
    H*                                                               *
    H* 07/23/10 C:235 Support: 73547                                 *
    H*                Field dedBill accumulating deposit, category D *
    H*                instead of deductible, category  R             *
    H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
    H*                                                               *
    H* 08/20/08 C:136 Project: 114WQB                                *
    H*                Wachovia Lockbox Upload                        *
    H*                                                               *
    H* 03/06/06  C:33  Project: 71WGB                                *
    H*                 Process accounts paying more than one invoice *
    H*                  with the same check.                         *
    H*                                                               *
    H* 03/03/06  C:33  Project: 71WGB   WA071/51729                  *
    H*                 Process Amt1 prior to Amt2.  {No Comments}    *
    H*                  This code is being done in the WritePrem     *
    H*                  subroutine.                                  *
    H*                                                               *
    H* 02/17/06  C:33  Project: 71WGB   WA071/51420                  *
    H*                 Process invalid data.                         *
    H*                                                               *
    H* 01/27/06  C:33  Project: 71WGB     WA071/51116                *
    H*                 Be sure to load correct A/R code.             *
    H*                                                               *
    H* 10/17/05  C:33  Project: 71WGB                                *
    H*                 Add check digit verification                  *
    H*                                                               *
    H* 08/02/05  C:33  Project: 71WGB                                *
    H*                -Moved to CLT071                               *
    H*                -Convert to RPGIV                              *
    H*                -Remove comment lines                          *
    H*****************************************************************
     Fwxlul1    if   e           k disk
     F                                     infds(LUL1fds)
     FwxlupInv  if   e             disk    rename(wxlupinp:plu0pinv)
     F                                     usropn prefix(inv)
     Fwdeal     if   e           k disk    rename(pea01:pea00)
     Fwdeal4    uf a e           k disk    rename(pea01:pea04)
     F                                     infds(eafds)
     Fwmeal4    if   e           k disk    rename(pea01:pea04m)
     Fwmexl     if   e           k disk    rename(pex01:pex00)
     Fwmeml     if   e           k disk
     Fwdell     if   e           k disk    rename(pel01:pel00)
     Fwdell2    if   e           k disk    rename(pel01:pel02)
     Fwdf1l     if   e           k disk
     Fwmfnl     if   e           k disk
     Fwmupl     if   e           k disk
     Fsmorl     if   e           k disk
     Fsxcpl     if   e           k disk
     Fwtabl     if   e           k disk
     F                                     ignore(ppa01)
     F                                     ignore(pat01)
     F                                     ignore(psp01)
     F                                     ignore(pnc01)
     F                                     ignore(prc01)
     F                                     ignore(ppc01)
     F                                     ignore(pb201)
     F                                     ignore(pb701)
     F                                     ignore(pbs01)
     F                                     ignore(pcc01)
     F                                     ignore(pbc01)
     F                                     ignore(pcs01)
     F                                     ignore(pcn01)
     F                                     ignore(prs01)
     F                                     ignore(plr01)
     F                                     ignore(pvs01)
     F                                     ignore(ptr01)
     F                                     ignore(phy01)
     F                                     ignore(pba01)
     F                                     ignore(pid01)
     F                                     ignore(pis01)
     Fqsysprt   o    f  132        printer oflind(*inof)
     F                                     usropn
     *
     D*#ofErr***       c                   9
     D*#ofErr***       c                   10
     D #ofErr          c                   15
     D  err            s             73    dim(#ofErr) ctdata perrcd(1)
     D  txt            s             70    dim(#ofErr)
     D  wrc            s              7  0 dim(#ofErr)
     D weightSize      c                   50
     D weight          s              1    dim(weightSize) ctdata
     D                                     perrcd(weightSize)
     D amounts         s             11s 2

     D                 ds
     D LUCKDT                         8  0
     D yyyyLUCKDT                     4  0 overlay(LUCKDT)
     D mmddLUCKDT                     4  0 overlay(LUCKDT:5)

     D                 ds
     D xxErDt                              like(LUCKDT)
     D mmddxxErDt                     4  0 overlay(xxErDt)
     D yyyyxxErDt                     4  0 overlay(xxErDt:5)

     D LUSCNL          ds            80
     D  xxType                        1    overlay(LUSCNL)
     D  inpType                       1    overlay(LUSCNL)
     D  inpFnd                        3    overlay(LUSCNL:2)
     D  inpEmp#                       9    overlay(LUSCNL:5)
     D  inpDiv                        5    overlay(LUSCNL:14)
     D  inpFyr                        3    overlay(LUSCNL:19)
     D  inpAmt                       11    overlay(LUSCNL:22)
     D  inpFyr2                       3    overlay(LUSCNL:33)
     D  inpAmt2                      11    overlay(LUSCNL:36)
     D  chkDigit1                     1    overlay(LUSCNL:47)
     D  chkDigit2                     1    overlay(LUSCNL:47)
     D  chkDigit3                     1    overlay(LUSCNL:33)
     d  wachType                      1    overlay(LUSCNL:80)

     DinvWXLUP         ds
     D invCo#                         3    overlay(invWXLUP)
     D invChk#                        7    overlay(invWXLUP:4)
     D invCkAm                        9    overlay(invWXLUP:11)
     D invCkDt                        8    overlay(invWXLUP:20)
     D invScnL                       80    overlay(invWXLUP:28)
     D invType                        1    overlay(invWXLUP:108)
     D  invTypeScnL                   1    overlay(invScnL)
     D  invFnd                        3    overlay(invScnL:2)
     D  invEmp#                       9    overlay(invScnL:5)
     D  invDiv                        5    overlay(invScnL:14)
     D  invFyr                        3    overlay(invScnL:19)
     D  invAmt                       11    overlay(invScnL:22)
     D  invFyr2                       3    overlay(invScnL:33)
     D  invAmt2                      11    overlay(invScnL:36)
    D*
    D* Constants
    D*
     D title           c                   'UPLOAD LOCKBOX INFORMATION - ERROR-
     D                                      REPORT'
     D noGrpFound      c                   '***** NO GROUP FOUND *****'
     D noEmpFound      c                   '***** NO EMPLOYER FOUND *****'
     D psds          esds                  extname(szq1p)

     D LUL1fds         ds
     D  LUrrn1               397    400b 0

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D errAmt          s                   like(EAAMT)
     D errCount        s              7  0
     D errDesc         s            120
     D error           s              1n
     D inCount         s              7  0
     D n               s              3  0
     D postAmt         s                   like(EAAMT)
     D postCount       s              7  0
     D premFyr1        s                   like(ELFYR)
     D premAmt1        s                   like(amounts)
     D premFyr2        s                   like(ELFYR)
     D premAmt2        s                   like(amounts)
     D dedAmt1         s                   like(amounts)
     D premFyr         s                   like(premFyr1)
     D premAmt         s                   like(premAmt1)
     D prtReport       s              1n
     D prtTitle        s             60
     D prtFnd          s              3
     D prtFndName      s             29
     D prtEmp#         s              9
     D prtDiv          s              5
     D prtEmpName      s             29
     D prtFyr          s              3
     D prtCkDt         s             10
     D prtChk#         s              7
     D prtCkAm         s             12
     D prtAmt          s             14
     D qqco#           s              3
     D saveChk#        s                   like(LUCHK#)
     D saveChkAmt      s                   like(LUCKAM)
     D saveChkDt       s                   like(LUCKDT)
     D saveDiv         s                   like(inpDiv)
     D saveEmp#        s                   like(inpEmp#)
     D saveFnd         s                   like(inpFnd)
     D saveFndName     s             60
     D saveScanLine    s                   like(LUSCNL)
     D size            s              3  0
     D sysDate         s               d   datfmt(*USA)
     D sysTime         s               t   timfmt(*USA)
     D x               s              3  0
     D xxAmt           s                   like(EAAMT)
     D xxBat#          s                   like(EABAT#)
     D xxcamt          s                   like(LUCKAM)
     D xxchk#          s                   like(LUCHK#)
     D xxChkAmt        s                   like(amounts)
     D xxco#           s                   like(EACO#)
     D xxdata          s             60
     D xxDiv           s                   like(ELDIV)
     D xxEmp#          s                   like(ELEMP#)
     D xxEmpName       s             29
     D xxFnd           s                   like(ELFND)
     D xxFndName       s             29
     D xxFyr           s                   like(ELFYR)
     D xxkey           s                   like(TTKEY)
     D xxPFDt          s                   like(ELPFDT)
     D xxprt           s              1
     D xxstat          s                   like(EASTAT)
     D xxSysCd         s                   like(EASYCD)
     D xxTotBat        s                   like(EABATA)
     D dateChk         s               d
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     *=====================================================================
      * Wachovia Upload (114WQB) Field Definitions - TOP
     *=====================================================================
     d eafds           ds
     d  earrn                397    400b 0

     d DisolveCash     pr
     d cashDissolve    pr                  extpgm('WA055R')
     d  modeFlag                      1                 const
     d  rrn                           9  0              const
     d cashDslvAPI     pr                  extpgm('WA083R')
     d  co#                           3    const
     d  fnd                           3    const
     d  pgm                           1    const
     d  retCode                       1
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
     d clientID        s              3    inz('000')
     d UseCashDisslv   pr              n
     d ProcessRcd      pr
     d  xxType                        1
     *=====================================================================
      * Wachovia Upload (114WQB) Field Definitions - BOTTOM
     *=====================================================================
     DCEEDOD           PR
     D parmNum                       10I 0 const
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               10I 0
     D                               12A   options(*omit)
     D*
     D* parameters passed to CEEDOD
     D*
     D descType        s             10I 0
     D dataType        s             10I 0
     D descInfo1       s             10I 0
     D descInfo2       s             10I 0
     D inLen           s             10I 0
     D hexLen          s             10I 0

     DCvtNum           pr                  extPgm('WA003R')
     D number                        80
     D editMask                      10

     DEditCode         pr            25    opdesc
     D alpha                         15    options(*varsize) const
     D code                           1                      const

     DCheckDigit       PR             1
     D lineArray                      1    dim(weightSize) const
     D lineSize                       3  0                 const

     DCheckDigit1      PR             1
     D scanLine                            like(LUSCNL) const
     D
     DCheckDigit2      PR             1
     D scanLine                            like(LUSCNL) const
     D
     DCheckDigit3      PR             1
     D scanLine                            like(LUSCNL) const

     DIsoToDate        PR              d
     D numDate8                       8  0 Const

     DValidChkDigit    PR              n
     D scanLine                            like(LUSCNL) Const
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz

     DWriteRcds        PR
     D xxType                         1    options(*nopass) const

     D/copy SPrRtvBrnd
     D/copy sprDate
     D/copy sprString

     C     keyWDEAL4     klist
     C                   kfld                    xxco#
     C                   kfld                    xxBat#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C                   kfld                    xxFyr
    C*
     C     key2WDEAL4    klist
     C                   kfld                    xxco#
     C                   kfld                    xxBat#
    C*
     C     key3WDEAL4    klist
     C                   kfld                    xxco#
    C*
     C     exky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxFnd
    C*
     C     keyTable      klist
     C                   kfld                    xxco#
     C                   kfld                    xxkey
    C*
     C     keyWXLUL1     klist
     C                   kfld                    xxco#
    C*
     C     fnky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxFnd
    C*
     C     f1ky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxFnd
     C                   kfld                    xxFyr
    C*
     C     emky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
    C*
     C     keyWDELL      klist
     C                   kfld                    xxco#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C                   kfld                    xxFyr
    C*
     C     keyWDELL2     klist
     C                   kfld                    xxco#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C                   kfld                    xxPFDt
    C*
     C     key1WDELL2    klist
     C                   kfld                    xxco#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
    C*
    C*  WMUPL - User Parameters
    C*
     C     upky          klist
     C                   kfld                    q1user
    C*
     C     *entry        plist
     C                   parm                    qqco#
     C                   parm                    qqdissolve        1

     C                   exsr      Initialize
     c     xxCo#         setll     plu01
     c     xxCo#         reade     plu01
     c                   dow       not(%eof)
     C                   exsr      EditUpload
     c                   if        error
     c     xxCo#         reade     plu01
     c                   iter
     c                   endif
      *
     * Create batch#, if needed
      *
     c                   if        xxBat# = *zero
     c                   exsr      RtvNextBat#
     c                   endif
    
     c                   if        useCashDisslv
     c                   callp     DisolveCash
     c                   else
     c                   callp     ProcessRcd(xxType)
     c                   endif
    
     c     xxCo#         reade     plu01
     c                   enddo

     c                   exsr      UpdBtchTot
     c                   exsr      DltDummyRcd
    
     c                   if        prtReport
     c                   except    total
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If %open(qsysprt);
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c                   close     qsysprt
     c                   endif
    
     c                   move      *on           *inlr
    C*====================================================================
    C* Check for invalid fields
    C*====================================================================
     C     ChkValidFlds  begsr

     C                   select
     C                   when      inpFnd = *all'+'
     C                   clear                   xxFnd
     C                   other
     C                   move      inpFnd        xxFnd
     C                   endsl

     C                   select
     C                   when      inpEmp# = *all'+'
     C                   clear                   xxEmp#
     C                   other
     C                   move      inpEmp#       xxEmp#
     C                   endsl

     C                   select
     C                   when      inpDiv = *all'+'
     C                   clear                   xxDiv
     C                   other
     C                   move      inpDiv        xxDiv
     C                   endsl

     C                   select
     C                   when      inpFyr = *all'+'
     C                   clear                   xxFyr
     C                   clear                   premFyr1
     C                   other
     C                   move      inpFyr        xxFyr
     C                   move      inpFyr        premFyr1
     C                   endsl

     C                   select
     C                   when      inpAmt = *all'+'
     C                   clear                   xxAmt
     C                   clear                   premAmt1
     C                   clear                   dedAmt1
     C                   other
     C                   move      inpAmt        xxAmt
     C                   move      inpAmt        premAmt1
     C                   move      inpAmt        dedAmt1
     C                   endsl

     C                   select
     C                   when      inpFyr2 = *all'+'
     C                   clear                   premFyr2
     C                   other
     C                   move      inpFyr2       premFyr2
     C                   endsl

     C                   select
     C                   when      inpAmt2 = *all'+'
     C                   clear                   premAmt2
     C                   other
     C                   move      inpAmt2       premAmt2
     C                   endsl

     C                   endsr
    C*====================================================================
    C* Delete the A/R batch dummy record
    C*====================================================================
     C     DltDummyRcd   begsr
    
     C                   move      *hival        xxFnd
     C                   move      *hival        xxEmp#
     C                   move      *hival        xxDiv
     C                   move      *hival        xxFyr
     C     keyWDEAL4     delete    pea04                              60
    
     C                   endsr
    C*====================================================================
    C* Edit upload record for errors
    C*====================================================================
     C     EditUpload    begsr
    C*
     C                   eval      error = *off
     C                   clear                   txt
     C                   clear                   error
     C                   clear                   xxEmpName
     C*
     C                   if        LUTYPE = 'X'
     C                   eval      error   = *on
     C                   eval      txt(11) = err(11)
     C                   endif
     C*
     C                   exsr      ChkValidFlds
     C*
    C* Invalid Check Digit
     C*
     C                   if        not ValidChkDigit(LUSCNL)
     c                             and (wachType <> 'W' AND wachType <> 'L')
     C                   eval      error   = *on
     C                   eval      txt(10) = err(10)
     C                   endif
    C*
    C* Invalid Group
    C*
     C     fnky          chain     pfn01
     C*
     C                   if        not %found
     C                   eval      error     = *on
     C                   eval      txt(1)    = err(1)
     C                   eval      saveFnd    = inpFnd
     C                   eval      prtFndName = noGrpFound
     C*
     C                   else
     C*
     C                   eval      saveFndName = FNNAME
     C                   eval      prtFndName  = FNNAME
     C                   eval      saveFnd     = inpFnd
    C*
    C* Invalid Policy Year
    C*
     C                   select
     C                   when      xxType = '1' or xxType = '2'
     C     f1ky          chain     pf101
     C                   if        not %found
     C                   eval      error = *on
     C                   eval      txt(3) = err(3)
     C                   endif
     C                   when      xxType = '3'
     C                   endsl
    C*
    C* Employer Does not Exist
    C*
     C     emky          chain     pem01
     C                   if        not %found
     C                   eval      error = *on
     C                   eval      txt(4) = err(4)
     C                   eval      prtEmpName = noEmpFound
     C                   else
     C                   eval      prtEmpName = EMNAME
    C*
    C* Policy Year does not Exist
    C*
     C                   select
     C                   when      xxType = '1' or xxType = '2'
     C     keyWDELL      chain     pel00
     C                   if        not %found
     C                   eval      error = *on
     C                   eval      txt(5) = err(5)
     C                   endif
     C                   when      xxType = '3'
     C                   eval      xxPFDt = DateTo7(IsoToDate(LUCKDT))
     C     keyWDELL2     setgt     pel02
     C     key1WDELL2    readpe    pel02
     C                   if        %eof
     C                   eval      error = *on
     C                   eval      txt(9) = err(9)
     C                   endif
     C                   eval      xxFyr = ELFYR
     C                   endsl
    C*
     C                   endif
    C*
     C                   endif
    C*
    C* Trans/Posting date Invalid
    C*
     C                   eval      xxErDt = DtCheck(DateTo8(IsoToDate(LUCKDT)))
    C*
     C                   if        xxErDt = *zero
     C                   eval      mmddxxErDt = mmddLUCKDT
     C                   eval      yyyyxxErDt = yyyyLUCKDT
     C                   eval      error = *on
     C                   eval      txt(6) = err(6)
     C                   endif
    C*
    C* Check number Less than Zero
    C*
     C                   if        LUCHK# < *zeros
     C                   eval      error = *on
     C                   eval      txt(7) = err(7)
     C                   endif
    C*
    C* Check Amount Less than Zero
    C*
     C                   if        LUCKAM < *zeros
     C                   eval      error = *on
     C                   eval      txt(8) = err(8)
     C                   endif
    C*
    C* If have Error, print them
    C*
     C                   if        error
     C                   exsr      PrtErrors
     C                   endif
    C*
     C                   endsr
     C*====================================================================
    C* Load intial values
     C*====================================================================
     C     Initialize    begsr
    C*
     C                   move      qqco#         xxco#
     C                   move      qqdissolve    xxdissolve        1
     C                   eval      sysDate = SystemDate
     C                   eval      prtTitle = CenterFld(title:60)
     C                   eval      xxBat#   = 0
     C                   eval      xxTotBat = 0
     C                   eval      sysTime  = SystemTime
    C*
    C* READ THE LOCKBOX UPLOAD FILE FOR DATA.
    C*
     C                   eval      xxchk#     = *zeros
     C                   eval      saveChk#   = *zeros
     C                   eval      saveChkAmt = *zeros
     C                   eval      saveChkDt  = *zeros
     C                   eval      saveFnd    = *zeros
     C                   eval      saveEmp#   = *zeros
     C                   eval      saveDiv    = *zeros
     C                   eval      inCount   = *zeros
     C                   eval      errCount  = *zeros
     C                   eval      postCount = *zeros
     C                   eval      errAmt    = *zeros
     C                   eval      postAmt   = *zeros
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
      /free
       GetClientID(clientID);
      /end-free
    C*
     C                   endsr
    C*================================================================
    C* Load print fields
    C*================================================================
     C     LoadPrtFlds   begsr
     C*
     C                   eval      prtFnd  = EditCode(inpFnd:'3')
     C                   eval      prtEmp# = EditCode(inpEmp#:'3')
     C                   eval      prtDiv  = EditCode(inpDiv:'3')
     C                   eval      prtFyr  = EditCode(inpFyr:'3')
     C                   eval      prtAmt  = EditCode(inpAmt:'J')
     C*
     C                   select
     C                   when      LUCHK# = *zeros and LUTYPE = 'X'
     C                   eval      prtChk# = *all'+'
     C                   other
     C                   eval      prtChk# = %editc(LUCHK#:'3')
     C                   endsl
     C*
     C                   select
     C                   when      LUCKAM = *zeros and LUTYPE = 'X'
     C                   eval      size = %size(prtCkAm)
     C                   eval      %subst(prtCkAm:1:size) = *all'+'
     C                   other
     C                   eval      prtCkAm = %editc(LUCKAM:'J')
     C                   endsl
     C*
     C                   select
     C                   when      LUCKDT = *zeros and LUTYPE = 'X'
     C                   eval      prtCkDt = *all'+'
     C                   other
     C                   eval      prtCkDt = %editw(xxErDt:'  /  /    ')
     C                   endsl
    C*
     C                   endsr
    C*================================================================
    C* Print errors
    C*================================================================
     C     PrtErrors     begsr
    C*
     C                   if        not prtReport
     C                   exsr      RouteInfo
     C                   eval      prtReport = *on
     C                   endif
    C*
     C                   if        *inof = *on
     C                   except    head
     C                   endif
    C*
     C                   exsr      LoadPrtFlds
    C*
     C                   except    detail
    C*
     C                   do        #ofErr        x
     C                   if        txt(x) <> *blanks
     C                   select
     C                   when      x = 10
     C                   eval      errDesc = %trim(txt(x)) + ' - ' + LUSCNL
     C                   when      x = 11
     C                   open      WXLUPINV
     C     LUrrn1        chain     WXLUPINV
     C                   eval      errDesc = %trim(txt(x)) + '  - ' + invScnL
     C                   close     WXLUPINV
     C                   other
     C                   eval      errDesc = txt(x)
     C                   endsl
     C                   except    errlin
     C                   endif
     C                   enddo
    C*
     C                   except    spacer
    C*
     C                   eval      errCount = errCount + 1
     C                   eval      errAmt   = errAmt + LUCKAM
      *** beg del ***** 01/06/15 *********************************
     C***********        eval      xxTotBat = xxTotBat + LUCKAM
      *** beg del ***** 01/06/15 *********************************
    C*
     C                   endsr
    C*================================================================
    C* Get routing information
    C*================================================================
     C     RouteInfo     begsr
    C*
     C     orky          klist
     C                   kfld                    q1pgm
     C                   kfld                    xxprt
     *
     C                   move      '0'           xxprt
     C     orky          chain     por01
     C                   eval      ORTITL = LeftJstify(ORTITL)
    C*
     C                   if        not %found
     C                   eval      ORDIS1 = q1user
     C                   eval      ORPNAM = q1pgm
     C                   eval      ORFIL# = '0'
     C                   eval      ORCOPY = 1
     C                   eval      ORHOLD = '*NO'
     C                   eval      ORFORM = '*STD'
     C                   eval      ORSAVE = '*NO'
     C                   eval      ORTITL = title
     C                   endif
    C*
     C     upky          chain     pup01
     C                   if        %found
     C                   eval      OROUTQ = UPOUTQ
     C                   endif
    C*
     C                   open      qsysprt
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     C                   eval      *inof = *on
    C*
     C                   endsr
     C*====================================================================
    C* Update all batch records created with the total batch amount
     C*====================================================================
     C     UpdBtchTot    begsr
    C*
     C     key2WDEAL4    chain     pea04                              60
     C                   dow       *in60 = *off
      *** beg chg ***** 01/06/15 **********************************
     c***********        select
     c***********        when      wachType = 'W' or wachtype = 'L'
     c***********        eval      EABATA = xxTotBAT
     c***********        other
     c***********        eval      EABATA = postAmt
     c***********        endsl
     c                   eval      EABATA = xxTotBAT
      *** end chg ***** 01/06/15 **********************************
     C                   update    pea04
     C     key2WDEAL4    reade     pea04                                  60
     C                   enddo
    C*
     C                   endsr
     C*====================================================================
    C* Retrieve next available batch number
     C*====================================================================
     C     RtvNextBat#   begsr
     C                   eval      xxBat# = *hival
     C                   eval      EABAT# = *zero
     C     key2WDEAL4    setgt     pea04
     C     key3WDEAL4    readpe    pea04
     C                   eval      xxBat# = 1 + EABAT#
    C*
    C*  Write an A/R batch dummy record, to hold the bat#.
    C*
     C                   clear                   pea04
     C                   eval      EACO#  = xxco#
     C                   eval      EABAT# = xxBat#
     C                   eval      EABATA = 0
     C                   move      *hival        EAFND
     C                   move      *hival        EAFYR
     C                   move      *hival        EAEMP#
     C                   move      *hival        EADIV
     C                   move      'N'           eatfap
     C                   move      'N'           eatffl
     C                   move      q1user        eauser
     C                   move      q1pgm         eapgm
     C     sysTime       adddur    1:*s          sysTime                99
     C                   eval      EATIME = TimeTo6(sysTime)
     C                   eval      EADT  = DtCYMD(DateTo7(SystemDate))
     C                   eval      EAIDT = EADT
     C                   move      q1user        eaiusr
     C                   move      q1pgm         eaipgm
     C                   write     pea04
     *
     *  Find WCMS System Entry Code for A/R Batch Record
     *
     c                   read      psy01
     c                   dow       not(%eof)
     c                   if        SYtype = 'Y'
     c                   eval      xxSysCd = SYkey
     c                   leave
     c                   endif
     c                   read      psy01
     c                   enddo

     C                   endsr
     C*====================================================================
    C* O-Spec's
     C*====================================================================
     Oqsysprt   e            route          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********e            route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********e            route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********e            route          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********e            route          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********e            route          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********e            route          1
     O**********                                 15 'ROUTE TO:'
     O**********             ordis1              60
     O**********e            route          1
     O**********             ordis2              60
     O**********e            route          1
     O**********             ordis3              60
     O**********e            route          2
     O**********             ordis4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          e            route          1
     O                                           15 'ROUTE TO:'
     O                       ordis1              60
     O          e            route          1
     O                       ordis2              60
     O          e            route          1
     O                       ordis3              60
     O          e            route          2
     O                       ordis4              60
      ***** End Add ***** 08/05/15 ************************************
     O          e            route          2
     O                                           19 'REPORT TITLE:'
     O                       ortitl              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********e            route          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             orpnam              30
     O**********                                 36 'FILE:'
     O**********             orfil#              38
     O**********                                 58 'COPIES:'
     O**********             orcopy        z     62
     O**********                                 72 'HOLD:'
     O**********             orhold              77
     O**********e            route          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             oroutq              30
     O**********                                 37 'FORMS:'
     O**********             orform              48
     O**********                                 56 'SAVE:'
     O**********             orsave              61
      ***** End Del ***** 08/26/14 ************************************
     O          e            route          2
     O                                           19 'JOB DATE  . :'
     O                       sysDate             30
     O          e            route          2
     O                                           19 'RUN TIME/DTE:'
     O                       sysTime           +004
     O                                         +001 '&'
     O                       sysDate           +001
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     Oqsysprt   e            head           1 03
     O                       sysTime              8
     O                       prtTitle            96
     O                       sysDate            123
     O          e            head           2
     O                       q1pgm               10
     O                       page          z    123
     O                                          118 'PAGE'
     O          e            head           1
     O                                           92 'POSTING/'
     O                                          101 'CHECK'
     O                                          115 'CHECK'
     O                                          131 'POSTING'
     O          e            head           2
     O                                            3 'GRP'
     O                                           12 'GRP NAME'
     O                                           43 'EMPLOYER#'
     O                                           48 'DIV'
     O                                           62 'EMPLOYER NAME'
     O                                           82 'YEAR'
     O                                           93 'TRANS DATE'
     O                                          102 'NUMBER'
     O                                          116 'AMOUNT'
     O                                          130 'AMOUNT'
    O*
     O          e            detail         1
     O                       prtFnd               3
     O                       prtFndName          33
     O                       prtEmp#             43
     O                       prtDiv              48
     O                       prtEmpName          78
     O                       prtFyr              82
     O                       prtCkDt             93
     O                       prtChk#            102
     O                       prtCkAm            117
     O                       prtAmt             132
     O          e            errlin         1
     O                       errDesc            130
     O          e            spacer         1
    O*
     O          e            total       1  2
     O                                           24 '***** TOTALS *****'
     O                                         +  5 '******** UPLOAD LOCKBOX'
     O                                         +  1 'RECORDS ********'
    O*
     O          e            total          1
     O                                           22 'Total number of Upload'
     O                                           44 ' Lockbox records read:'
     O                       inCount       3     53
     O                                           80 'Total All Check Amts :'
     O                       xxTotBat      j     95
     O          e            total          1
     O                                           22 'Total number-Records w'
     O                                           44 'ith Errors Listed  . :'
     O                       errCount      3     53
     O                                           80 'Total Error Check Amt:'
     O                       errAmt        j     95
     O          e            total          1
     O                                           22 'Total number-Records P'
     O                                           44 'osted w/NO Errors  . :'
     O                       postCount     3     53
     O                                           80 'Total Posted Amount  :'
     O                       postAmt       j     95
     P*=====================================================================
     P* CheckDigit - Calculate the check digit
     P*=====================================================================
     PCheckDigit       B
     DCheckDigit       PI             1
     D lineArray                      1    dim(50) const
     D lineSize                       3  0         const
     D
     D a               s              2  0
     D xxln            s              1  0
     D xxrn            s              1  0
     D xx2n            s              2  0
     D xxRem           s              1  0
     D xxSum           s              5  0 inz
     D xxWork          s              2  0

    C* Check Digit
     C                   do        lineSize      a
     C                   testn                   lineArray(a)         60
     C                   if        not *in60
     C                   leave
     C                   endif
     C                   move      lineArray(a)  xxln
     C                   move      weight(a)     xxrn
     C                   eval      xx2n = xxln * xxrn
     C                   movel     xx2n          xxln
     C                   move      xx2n          xxrn
     C                   eval      xxSum = xxSum + xxln + xxrn
     C                   enddo

     C     xxSum         div       10            xxWork
     C                   mvr                     xxrem

     C                   select
     C                   when      a <= lineSize
     C                   return    '*'
     C                   other
     C                   return    %subst(%editc((10 - xxRem):'X'):3)
     C                   endsl

     PCheckDigit       E
     P*=====================================================================
     P* CheckDigit1 - Calculate the check digit for type 1
     P*=====================================================================
     PCheckDigit1      B
     DCheckDigit1      PI             1
     D scanLine                            like(LUSCNL) Const
     D
     D lineSize        c                   46
     D lineArray       s              1    dim(50)
     D scanLine2       s             46

    C* Check Digit
     C                   eval      scanLine2 = scanLine
     C                   movea     scanLine2     lineArray
     C                   return    CheckDigit(lineArray:lineSize)

     PCheckDigit1      E
     P*=====================================================================
     P* CheckDigit2 - Calculate the check digit for type 2
     P*=====================================================================
     PCheckDigit2      B
     DCheckDigit2      PI             1
     D scanLine                            like(LUSCNL) Const

     C                   return    CheckDigit1(scanLine)

     PCheckDigit2      E
     P*=====================================================================
     P* CheckDigit3 - Calculate the check digit for type 3
     P*=====================================================================
     PCheckDigit3      B
     DCheckDigit3      PI             1
     D scanLine                            like(LUSCNL) Const
     D
     D lineSize        c                   32
     D lineArray       s              1    dim(50)
     D scanLine2       s             32

    C* Check Digit
     C                   eval      scanLine2 = scanLine
     C                   movea     scanLine2     lineArray

     C                   return    CheckDigit(lineArray:lineSize)

     PCheckDigit3      E
     P*=====================================================================
     P* Makeshift edit code operation
     P*=====================================================================
     PEditCode         b                   export
     DEditCode         pi            25    opdesc
     D inAlpha                       15    options(*varsize) const
     D code                           1                      const

     D alpha           s                   inz like(inAlpha)
     D converted       s             25
     D cvtAlpha        s             80    inz
     D editMask        s             10
     D extra           s              3  0
     D num             s             15  0
     D sizeAlpha       s              3  0
     D sizeNum         s              3  0

     C                   callp     CEEDOD(1:descType:dataType:descInfo1:
     C                             descInfo2:inLen:*omit)
     C                   eval      sizeAlpha = inLen
     C                   eval      sizeNum   = %len(Num)

     C                   if        %subst(inAlpha:1:sizeAlpha) = *all'+'
     C                   select
     C                   when      code = '3'
     C                   return    %subst(inAlpha:1:sizeAlpha)
     C                   when      code = 'J'
     C                   eval      extra = sizeAlpha / 3
     C                   eval      %subst(alpha:1:sizeAlpha+extra) = *all'+'
     C                   return    alpha
     C                   when      code = 'X'
     C                   return    %subst(inAlpha:1:sizeAlpha)
     C                   endsl
     C                   endif

     C                   eval      alpha = %subst(inAlpha:1:sizeAlpha)
     C                   move      alpha         num
     C                   eval      cvtAlpha = alpha
     C*
     C                   select
     C                   when      code = '3'
     C                   eval      converted = %editc(num:'3')
     C                   when      code = 'J'
     C                   eval      editMask = 'X,XXX.XX'
     C                   callp     CvtNum(cvtAlpha:editMask)
     C                   movel     cvtAlpha      converted
     C                   when      code = 'X'
     C                   eval      converted = %editc(num:'X')
     C                   endsl
     C
     C                   return    converted
     PEditCode         e
     P*=====================================================================
     P* IsoToDate - convert *ISO number to date data type
     P*  Parameter 1 - 8 numeric date
     P*  Returns - date data type (date will be 01/01/0001 if invalid)
     P*=====================================================================
     PIsoToDate        B                   Export
     DIsoToDate        PI              d
     D numDate8                       8  0 Const
     D
     D date1           s               d
     D year            s              4  0
     C*
     C     *iso          test(d)                 numDate8               99
     C  n99*iso          move      numDate8      date1
     C
     C                   return    date1
     PIsoToDate        E
     P*=====================================================================
     P* ValidChkDigit - Valid the check digit
     P*=====================================================================
     PValidChkDigit    B
     DValidChkDigit    PI              n
     D scanLine                            like(LUSCNL) Const
     D
     D valid           s               n   inz

     C                   select
     C                   when      xxType = '1'
     C                   return    CheckDigit1(scanLine) = chkDigit1
     C                   when      xxType = '2'
     C                   return    CheckDigit2(scanLine) = chkDigit2
     C                   when      xxType = '3'
     C                   return    CheckDigit3(scanLine) = chkDigit3
     C                   endsl

     C                   return    valid

     PValidChkDigit    E
     P*====================================================================
     P* Write A/R batch records
     P*====================================================================
     PWriteRcds        B
     DWriteRcds        PI
     D xxType                         1    options(*nopass) const

     D holdFyr         s                   like(ELFYR)
     D type            s              1
     D dedAmt          s                   like(amounts)
     D dedBill         s                   like(amounts)
     D useAmt          s                   like(amounts)
     D xxARType        s                   like(EAARTY)

     C     keyWMEAL4     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxFnd
     C                   kfld                    xxEmp#
     C                   kfld                    xxDiv
     C                   kfld                    xxFyr
     C                   kfld                    xxARType

     C                   eval      type = '1'
     C                   if        %parms >= 1
     C                   eval      type = xxType
     C                   endif

     C                   clear                   pea04
     C                   clear                   EXBLAR
     C                   clear                   EXATAR
     C                   clear                   EXDDAR
     C     exky          chain     pex00
     C                   select
     c                   when      clientID = '129'
     c     exky          chain     scp01
     c                   eval      EAarcd = XC10a4
     C                   when      type = '1'
     C                   eval      EAARCD = EXBLAR
     C                   when      type = '2'
     C                   eval      EAARCD = EXATAR
     C                   when      type = '3'
     C                   eval      EAARCD = EXDDAR
     C                   endsl
    C*
     C                   movel(p)  EAARCD        xxkey
     C     keyTable      chain     ptt01                              60
     C                   if        *in60 = *off
     C                   movel     TTDESC        EADESC
     c                   eval      EActyp = TTctyp
     C                   endif
     C*
     C                   select
     C                   when      type = '1'
     C                   exsr      Write1
     C                   when      type = '2'
     C                   exsr      Write2
     C                   when      type = '3'
     C                   exsr      Write3
     C                   endsl
     C*
     C                   return
     C*====================================================================
    C* Write A/R records for type = 1
     C*====================================================================
     C     Write1        begsr
     c                   move      inpFnd        EAFND
     c                   move      inpEmp#       EAEMP#
     c                   move      inpDiv        EADIV
     c                   move      LUCHK#        EACHK#
     c                   move      LUCKAM        EACKAM
     C                   eval      EACO#  = xxco#
     C                   eval      EABAT# = xxBat#
     C                   eval      EAFYR  = premFyr
     C                   eval      EATRDT = dtCYMD(dateTo8(cvtToDate(LUckdt)))
     C                   eval      EAPODT = dtCYMD(dateTo8(cvtToDate(LUckdt)))
     C                   eval      EAAMT  = premAmt
     C                   eval      EASYCD = xxSysCd
     C                   eval      EATFAP = 'N'
     C                   eval      EATFFL = 'N'
     C                   eval      EATFDT = *zeros
     C                   eval      EATFAM = *zeros
     C                   eval      EABATA = *zeros
     C                   eval      EAINV# = *zeros
     C                   clear                   EALATE
     C                   clear                   EABB#
     C                   eval      EAACHF = 'L'
     C                   eval      EAACHD = *zeros
     C                   eval      EAACHT = *zeros
     C                   clear                   EAACHU
     C                   clear                   EASTAT
     C                   eval      EAUSER = q1user
     C                   eval      EAPGM  = q1pgm
     C     sysTime       adddur    1:*s          sysTime                99
     C                   eval      EATIME = TimeTo6(sysTime)
     C                   eval      EADT   = DtCYMD(DateTo7(SystemDate))
     C                   eval      EAIDT  = EADT
     C                   eval      EAIUSR = q1user
     C                   eval      EAIPGM = q1pgm
     C                   write     pea04
     C                   eval      xxTotBat  = xxTotBat + EAAMT
     C                   eval      postAmt   = postAmt + EAAMT
     C                   eval      postCount = postCount + 1
     C                   endsr
     C*====================================================================
    C* Write A/R records for type = 2
     C*====================================================================
     C     Write2        begsr
     C                   exsr      Write1
     C                   endsr
     C*====================================================================
    C* Write A/R records for type = 3
     C*====================================================================
     C     Write3        begsr
     C                   eval      holdFyr = xxFyr
     C                   eval      dedAmt  = dedAmt1
     C*
     C* Dissolve check amount
     C*
     C                   dow       dedAmt > 0
     C                   clear                   dedBill
     C* Read A/R for 'F' type, 'D' category (WMEAL4)
     C                   eval      xxARType = 'F'
     C     keyWMEAL4     setll     pea04m
     C     keyWMEAL4     reade     pea04m
     C                   dow       not %eof
     C                   eval      xxKey = EAARCD
     C     keyTable      chain     ptt01
     C                   if        %found and TTCTYP = 'R'
     C                   eval      dedBill = dedBill + EAAMT
     C                   endif
     C     keyWMEAL4     reade     pea04m
     C                   enddo
     C*
     C                   eval      useAmt = dedAmt
     C                   if        dedBill < dedAmt
     C                   eval      useAmt = dedBill
     C                   endif
     C*
     C                   eval      dedAmt = dedAmt - dedBill
     C                   eval      premFyr = xxFyr
     C                   eval      premAmt = useAmt
     C                   exsr      Write1
     C                   eval      xxFyr = xxFyr - 1
     C     keyWDELL      chain     pel00
     C                   if        not %found
     C                   leave
     C                   endif
     C*
     C                   enddo
     C*
     C                   if        dedAmt > 0
     C                   eval      premFyr = holdFyr
     C                   eval      premAmt = dedAmt
     C                   exsr      Write1
     C                   endif
     C*
     C                   endsr
     PWriteRcds        E
      *=========================================================================
      * DisolveCash - write Wachovia Upload Record to Batch
      *=========================================================================
     p DisolveCash     b
     d rrn             s              9  0

     c                   clear                   pea04
     C                   eval      EACO#  = xxco#
     c                   move      inpFnd        EAFND
     c                   move      inpEmp#       EAEMP#
     c                   move      inpDiv        EADIV
     c                   move      inpFyr        EAFYR
     c                   move      LUCHK#        EACHK#
     c                   move      LUCKAM        EACKAM
      /Free
       EAbat# = xxbat#;
       EAco#  = xxco#;
       EAtrdt = DateTo7(SystemDate);
       EApodt = dtCYMD(dateTo8(cvtToDate(LUckdt)));
       EAsycd = xxSysCd;
       EAtfap = 'N';
       EAtffl = 'N';
       EAachf = 'L';
       EAtime = TimeTo6(SystemTime);
       EApgm  = Q1pgm;
       EAuser = Q1user;
       EAdt   = EAtrdt;
       EAiusr = Q1user;
       EAipgm = Q1pgm;
       EAidt  = EAtrdt;
       Write pea04;
       rrn    = EArrn;
       cashDissolve('L' : rrn);
       xxTotBat = xxTotBat + EAckam;
       //*** beg add ***** 01/06/15 ******
       postAmt  = postAmt  + EAckam;
       postCount = postCount + 1;
       //*** end add ***** 01/06/15 ******
      /End-Free
     p DisolveCash     e
      *===============================================================
      * UseCashDisslv - checks if transaction is going to be washed
      *                 through cash dissolve or not
      *===============================================================
     p UseCashDisslv   b
     d UseCashDisslv   pi              n
     d x1Dissolve      s              1
     d ##RtCd          s              1

      /Free
       x1Dissolve = *blanks;
       if xxDissolve = 'Y';
         Callp CashDslvAPI(%editc(xxCo# :'X'):
                           InpFnd:
                           'L':
                           ##RtCd);
         If ##RtCd = '0';
           x1Dissolve = ' ';
         Else;
           x1Dissolve = 'Y';
         Endif;                                                               //
       Endif;                                                                 //

       Select;
       When wachType = 'W';
         return *on;
       When xxDissolve = 'Y' and wachType = 'L' and
        x1Dissolve = 'Y';
         return *on;
       Other;
         return *off;
       Endsl;

      /End-free
     p UseCashDisslv   e
      *===============================================================
      * ProcessRcd - process record
      *===============================================================
     p ProcessRcd      b
     d ProcessRcd      pi
     d  xxType                        1
     d  xxCkAm         s                   like(LUckam)

      /Free
       xxCkAm = LUckam;
       Select;
       When xxType  = '1' or xxType = '2';
         premFyr = premFyr1;
         If xxCkAm  > premAmt1;
           premAmt = premAmt1;
           xxCkam  = xxCkAm - premAmt1;
         Else;
           premAmt = xxCkAm;
           xxCkam  = 0;
         Endif;
         If premAmt > 0;
           writeRcds(xxType);
         Endif;
         premFyr = premFyr2;
         If xxCkAm  > premAmt2;
           premAmt = premAmt2;
           xxCkam  = xxCkAm - premAmt2;
         Else;
           premAmt = xxCkAm;
           xxCkam  = 0;
         Endif;
         If premAmt > 0;
           writeRcds('2');
         Endif;
         If xxCkAm  > 0;
           premFyr = premFyr1;
           premAmt = xxCkAm;
           writeRcds(xxType);
         Endif;
       When xxType  = '3';
         writeRcds(xxType);
       Endsl;
      /End-Free

     p ProcessRcd      e

** ERR
01-INVALID GROUP NUMBER
02-INVALID EMPLOYER/DIVISON
03-INVALID POLICY YEAR
04-EMPLOYER DOES NOT EXIST FOR THE GROUP SPECIFIED
05-POLICY YEAR DOES NOT EXIST FOR THE GROUP/EMPLOYER/DIVISION SPECIFIED
06-POSTING DATE/TRANSACTION DATE IS INVALID
07-CHECK NUMBER IS LESS THAN ZERO
08-CHECK AMOUNT IS LESS THAN ZERO
09-CHECK DATE DOES NOT FALL WITHIN A POLICY YEAR FOR EMPLOYER
10-INVALID CHECK DIGIT
11-INVALID DATA FOUND
12-
13-
14-
15-
** weight                                                              ***
37137137137137137137137137137137137137137137137137
