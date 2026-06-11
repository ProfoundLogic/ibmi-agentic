     H Copyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('W8006R3')
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*   THIS PROGRAM PRODUCES A REPORT OF NEW CLAIMS AND CHANGED    *
     H*   STATUS CLAIMS FOR AN ENTERED DATE RANGE AND ONE OR ALL      *
     H*   EMPLOYERS.                                                  *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W8006C                                                      *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:33  Project: SY420
     H*                Mask SSN
     H*
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 09/01/11 C:65  ProjectL  WC1851                               *
     H*                Attach resolved report to DARTS remark of the  *
     H*                fee invoice as a secondary document if iDARTS. *
     H*                                                               *
     H* 05/20/11 C:190 WC1851 Priss 171                               *
     H*                Only display the last four digits of SSN#      *
     H*                if selected  4.                                *
     H*                                                               *
     H* 05/11/11 C:65  WC1851 Priss 176                               *
     H*                Do not accumulate the percentages, they should *
     H*                only be stored.                                *
     H*                                                               *
     H* 02/15/11 C:33  WC1851                                         *
     H*                Re-write                                       *
     H*****************************************************************
     Fwxfdl     if   e           k disk    rename(pfd01:xfd00)
     Fsmorl     if   e           k disk    rename(por01:por00)
     Fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     Fwdelp     if   e             disk    rename(pel01:pel0p)
     Fssylf     if   e           k disk     ignore(sbc01:scs01:src01:spc01)
     FSDCMLD    IF   E           K DISK    RENAME(PCM01:PCM02)
     F                                     PREFIX(C2)

      * Report Output
     Fqsysprt   O    f  198        printer oflInd(*inof)
     F                                     usrOpn
     Fqsysprt2  O    f  198        printer oflInd(*inoa)
     F                                     usrOpn

     D xxCoName        s             40
     D xxSS#           s             11
     D xxNewOrChgd     s             40
     D xxFeeDesc       s             40
     D xxFeeDescHdg    s             80
     D total1aHd       s             80
     D total1bHd       s             80
     D total1Emp       s             40
     D xxOutQ          s             10
     D xxQue           s             10
     D upUsNm          s             40
     D routeEmp        s             15
     D subHead         s              1n
     D saveFDFND       s                   like(FDFND )
     D saveFDFNAM      s                   like(FDFNAM)
     D saveFDEMP#      s                   like(FDEMP#)
     D saveFDDIV       s                   like(FDDIV )
     D saveFDENAM      s                   like(FDENAM)
     D saveFDTYPE      s                   like(FDTYPE)
     D saveFDDESC      s                   like(FDDESC)
     D saveFDBT        s                   like(FDBT  )
     D saveFDFYR       s                   like(FDFYR )
     D saveFDASTS      s                   like(FDASTS)
     D saveFDCASE      s                   like(FDCASE)
     D saveFDASOF      s                   like(FDASOF)
     D saveFDIVDT      s                   like(FDIVDT)
     D saveFDCAP#      s                   like(FDCAP#)
     D saveFDMINF      s                   like(FDMINF)
     D xtTbId          s                   like(GTTBID)
     D xfTbId          s                   like(GTTBID)
     D xtKey           s                   like(GTKEY)
     D totalLvl        s             10
     D Out_Typ         s              1  0
     D Out_Typ_Desc    s             25
     D x8LsDt          s              8  0
     D x8PoDt          s              8  0
     D x8PrDt          s              8  0
     D rrn             s              9  0
     D sysDate         s               d   datFmt(*usa)
     D sysTime         s               t   timFmt(*usa)
     D prtFinalTtl     s              1n   inz('0')
     D dltQsys1        s              1n   inz('1')
     D firstRcd        s              1n   inz('1')
     D prtGrpHdg       s              1n   inz('1')
     D prtEmpHdg       s              1n   inz('1')

     Dcount            s              3  0
     Dfees             s              9  2
     Dclf              ds                  inz
     D NewInd#                             like(count)
     D NewIndFee                           like(fees)
     D NewIndTot                           like(fees)
     D NewMed#                             like(count)
     D NewMedFee                           like(fees)
     D NewMedTot                           like(fees)
     D NewRcd#                             like(count)
     D NewRcdFee                           like(fees)
     D NewRcdTot                           like(fees)
     D New#                                like(count)
     D NewTot                              like(fees)
     D ChgIM#                              like(count)
     D ChgIMPrvFee                         like(fees)
     D ChgIMCurFee                         like(fees)
     D ChgIMAdj                            like(fees)
     D ChgIR#                              like(count)
     D ChgIRPrvFee                         like(fees)
     D ChgIRCurFee                         like(fees)
     D ChgIRAdj                            like(fees)
     D ChgMI#                              like(count)
     D ChgMIPrvFee                         like(fees)
     D ChgMICurFee                         like(fees)
     D ChgMIAdj                            like(fees)
     D ChgMR#                              like(count)
     D ChgMRPrvFee                         like(fees)
     D ChgMRCurFee                         like(fees)
     D ChgMRAdj                            like(fees)
     D ChgRI#                              like(count)
     D ChgRIPrvFee                         like(fees)
     D ChgRICurFee                         like(fees)
     D ChgRIAdj                            like(fees)
     D ChgRM#                              like(count)
     D ChgRMPrvFee                         like(fees)
     D ChgRMCurFee                         like(fees)
     D ChgRMAdj                            like(fees)
     D Chg#                                like(count)
     D ChgPrv                              like(fees)
     D ChgCur                              like(fees)
     D ChgAdj                              like(fees)
     DclfFyr           ds                  likeDS(clf) inz
     DclfEmp           ds                  likeDS(clf) inz
     DclfGrp           ds                  likeDS(clf) inz
     DclfGrpC          ds                  likeDS(clf) inz
     DclfGrpF          ds                  likeDS(clf) inz
     DclfGrpI          ds                  likeDS(clf) inz
     DclfRpt           ds                  likeDS(clf) inz
     DclfRptC          ds                  likeDS(clf) inz
     DclfRptF          ds                  likeDS(clf) inz
     DclfRptI          ds                  likeDS(clf) inz
     DclfTot           ds                  likeDS(clf) inz
     DclfTotC          ds                  likeDS(clf) inz
     DclfTotF          ds                  likeDS(clf) inz
     DclfTotI          ds                  likeDS(clf) inz
     D*
     Damt              s             11  2
     Dpercent          s              5  2
     Dgen              ds                  inz
     D PriPaid                             like(amt)
     D CurPaid                             like(amt)
     D ClmPdPct                            like(percent)
     D PriBillP                            like(amt)
     D CurBillP                            like(amt)
     D BilledP                             like(amt)
     D PriInc                              like(amt)
     D CurInc                              like(amt)
     D ClmIncPct                           like(percent)
     D PriBillI                            like(amt)
     D CurBillI                            like(amt)
     D BilledI                             like(amt)
     DgenFyr           ds                  likeDS(gen) inz
     DgenEmp           ds                  likeDS(gen) inz
     DgenGrp           ds                  likeDS(gen) inz
     DgenRpt           ds                  likeDS(gen) inz
     DgenTot           ds                  likeDS(gen) inz

     DxxFrom           s              8  0
     DxxTo             s              8  0
     DtotBill3         s                   like(amt)
     DtotBill4         s                   like(amt)
     DtotBill5         s                   like(amt)

      * Needed for copyright
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz

     DGetBillTpDesc    pr                  like(GTDESC)
     D type                                like(FDTYPE) const
     D key                                 like(GTKEY)  const
     D*
     DGetFeeDesc       pr                  like(GTDESC)
     D key                                 like(GTKEY)  const
     D tableId                             like(GTTBID) const
     D*
     D/copy sPrCompany
     D/copy sPrDate
     D/copy sPrRunCmd
     D/copy sPrRtvBrnd
     D/copy sPrString

     D/copy wPrEmplr
     D/copy wPrUsrPrms
      ***  Begin Add  ***  06/09/20  *******************
     D/copy sPrMask
     D/copy sPrSecure
      ***  End   Add  ***  06/09/20  *******************

     D title0          C                   CONST('FEE DETAIL REPORT')

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C     keySMORL      klist
     C                   kfld                    Q1PGM
     C                   kfld                    KKPRT             1
     C*
     C     keytWTGTL     klist
     C                   kfld                    FDCO#
     C                   kfld                    xtTbId
     C*
     C     keyt1WTGTL    klist
     C                   kfld                    FDCO#
     C                   kfld                    xtTbId
     C                   kfld                    xtKey
     C*
     C     keyfWTGTL     klist
     C                   kfld                    FDCO#
     C                   kfld                    xfTbId
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*   KEY LIST FOR THE DARTS FILE
     C*
     C     D1KY          KLIST
     C                   KFLD                    ##KEY            40
     C                   KFLD                    @@DRM

     C     *ENTRY        PLIST
     C                   PARM                    @@co#             3
     C                   PARM                    @@fnd             3
     C                   PARM                    @@emp#            9
     C                   PARM                    @@div             5
     C                   PARM                    @@podt            8
     C                   PARM                    @@prdt            8
     C                   PARM                    @@lsDT            8
     C                   PARM                    @@dlfg            1
     C                   PARM                    @@drm             5
     C                   PARM                    @@ssf             1
     C                   PARM                    @@excl            1
     C                   PARM                    @@outq           10
     C                   PARM                    @@pfb             1
      *
      *   Initialize
      *
     c                   eval      x8LsDt = DtCheck(%int(@@LsDt))
     c                   eval      x8PoDt = DtCheck(%int(@@PoDt))
     c                   eval      x8PrDt = DtCheck(%int(@@PrDt))
     c                   move      @@co#         kkco#             3 0
     c                   move      @@fnd         kkfnd             3 0
     c                   move      @@emp#        kkemp#            9 0
     c                   move      @@div         kkdiv             5 0

     C                   exsr      PgmStr
     C                   exsr      Routing
     C*
     C     *loval        setll     wxfdl
     C                   dou       %eof(wxfdl)
     C                   read      wxfdl
     C                   if        not %eof
     C                   exsr      ProcessRcds
     C                   endif
     C                   enddo
     C*
     C                   exsr      BrkGrp
     C                   eval      prtGrpHdg = *off
     C                   exsr      FinalTotals
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain keySMORL smorl;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
      *
      *  End of Processing
     C                   MOVE      *ON           *INLR
     C*
     C*====================================================================
     C* Template subroutine
     C*====================================================================
     C     Template      begsr
     C*
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process claim breaks
     C*====================================================================
     C     BrkCase       begsr
     C*
     C                   exsr      UpdtLastRcd
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process employer breaks
     C*====================================================================
     C     BrkEmp        begsr
     C*
     C                   exsr      BrkLayout
     C*
     C                   eval      clfGrp.NewInd#   += clfEmp.NewInd#
     C                   eval      clfGrp.NewIndFee += clfEmp.NewIndFee
     C                   eval      clfGrp.NewIndTot += clfEmp.NewIndTot
     C                   eval      clfGrp.NewMed#   += clfEmp.NewMed#
     C                   eval      clfGrp.NewMedFee += clfEmp.NewMedFee
     C                   eval      clfGrp.NewMedTot += clfEmp.NewMedTot
     C                   eval      clfGrp.NewRcd#   += clfEmp.NewRcd#
     C                   eval      clfGrp.NewRcdFee += clfEmp.NewRcdFee
     C                   eval      clfGrp.NewRcdTot += clfEmp.NewRcdTot
     C                   eval      clfGrp.New#      += clfEmp.New#
     C                   eval      clfGrp.NewTot    += clfEmp.NewTot
     C*
     C                   eval      clfGrp.ChgIM#      += clfEmp.ChgIM#
     C                   eval      clfGrp.ChgIMPrvFee += clfEmp.ChgIMPrvFee
     C                   eval      clfGrp.ChgIMCurFee += clfEmp.ChgIMCurFee
     C                   eval      clfGrp.ChgIMAdj    += clfEmp.ChgIMAdj
     C                   eval      clfGrp.ChgIR#      += clfEmp.ChgIR#
     C                   eval      clfGrp.ChgIRPrvFee += clfEmp.ChgIRPrvFee
     C                   eval      clfGrp.ChgIRCurFee += clfEmp.ChgIRCurFee
     C                   eval      clfGrp.ChgIRAdj    += clfEmp.ChgIRAdj
     C                   eval      clfGrp.ChgMI#      += clfEmp.ChgMI#
     C                   eval      clfGrp.ChgMIPrvFee += clfEmp.ChgMIPrvFee
     C                   eval      clfGrp.ChgMICurFee += clfEmp.ChgMICurFee
     C                   eval      clfGrp.ChgMIAdj    += clfEmp.ChgMIAdj
     C                   eval      clfGrp.ChgMR#      += clfEmp.ChgMR#
     C                   eval      clfGrp.ChgMRPrvFee += clfEmp.ChgMRPrvFee
     C                   eval      clfGrp.ChgMRCurFee += clfEmp.ChgMRCurFee
     C                   eval      clfGrp.ChgMRAdj    += clfEmp.ChgMRAdj
     C                   eval      clfGrp.ChgRI#      += clfEmp.ChgRI#
     C                   eval      clfGrp.ChgRIPrvFee += clfEmp.ChgRIPrvFee
     C                   eval      clfGrp.ChgRICurFee += clfEmp.ChgRICurFee
     C                   eval      clfGrp.ChgRIAdj    += clfEmp.ChgRIAdj
     C                   eval      clfGrp.ChgRM#      += clfEmp.ChgRM#
     C                   eval      clfGrp.ChgRMPrvFee += clfEmp.ChgRMPrvFee
     C                   eval      clfGrp.ChgRMCurFee += clfEmp.ChgRMCurFee
     C                   eval      clfGrp.ChgRMAdj    += clfEmp.ChgRMAdj
     C                   eval      clfGrp.Chg#        += clfEmp.Chg#
     C                   eval      clfGrp.ChgPrv      += clfEmp.ChgPrv
     C                   eval      clfGrp.ChgCur      += clfEmp.ChgCur
     C                   eval      clfGrp.ChgAdj      += clfEmp.ChgAdj
     C*
     C                   select
     C                   when      saveFDTYPE = 'CLF' and saveFDBT = 'C'
     C                   eval      clfGrpC = clfGrp
     C                   when      saveFDTYPE = 'CLF' and saveFDBT = 'F'
     C                   eval      clfGrpF = clfGrp
     C                   when      saveFDTYPE = 'CLF' and saveFDBT = 'I'
     C                   eval      clfGrpI = clfGrp
     C                   endsl
     C*
     C                   eval      genGrp.PriPaid     += genEmp.PriPaid
     C                   eval      genGrp.CurPaid     += genEmp.CurPaid
     c*** begin chg ***  05/11/11  **********************************************
     C*********          eval      genGrp.ClmPdPct    += genEmp.ClmPdPct
     c                   if        genEmp.ClmPdPct <> 0
     C                   eval      genGrp.ClmPdPct    = genEmp.ClmPdPct
     c                   endif
     c*** end chg *****  05/11/11  **********************************************
     C                   eval      genGrp.PriBillP    += genEmp.PriBillP
     C                   eval      genGrp.CurBillP    += genEmp.CurBillP
     C                   eval      genGrp.BilledP     += genEmp.BilledP
     C                   eval      genGrp.PriInc      += genEmp.PriInc
     C                   eval      genGrp.CurInc      += genEmp.CurInc
     c*** begin chg ***  05/11/11  **********************************************
     C*********          eval      genGrp.ClmIncPct   += genEmp.ClmIncPct
     c                   if        genEmp.ClmIncPct <> 0
     C                   eval      genGrp.ClmIncPct   = genEmp.ClmIncPct
     c                   endif
     c*** end chg *****  05/11/11  **********************************************
     C                   eval      genGrp.PriBillI    += genEmp.PriBillI
     C                   eval      genGrp.CurBillI    += genEmp.CurBillI
     C                   eval      genGrp.BilledI     += genEmp.BilledI
     C*
     C                   clear                   clfEmp
     C                   clear                   genEmp
     C*
     C                   exsr      Darts
     C*
     C                   eval      *inof = *on
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process policy year breaks
     C*====================================================================
     C     BrkFyr        begsr
     C*
     C                   eval      subHead = *off
     C                   select
     C                   when      saveFDTYPE = 'GEN'
     C                              and (saveFDBT = 'E'
     C                                    or saveFDBT = 'F'
     C                                    or saveFDBT = 'P')
     C                   other
     C                   eval      *inof = *on
     C                   endsl
     C*
     C                   eval      totalLvl = 'FYR'
     C                   exsr      PrintTotals
     C*
     C                   eval      clfEmp.NewInd#   += clfFyr.NewInd#
     C                   eval      clfEmp.NewIndFee += clfFyr.NewIndFee
     C                   eval      clfEmp.NewIndTot += clfFyr.NewIndTot
     C                   eval      clfEmp.NewMed#   += clfFyr.NewMed#
     C                   eval      clfEmp.NewMedFee += clfFyr.NewMedFee
     C                   eval      clfEmp.NewMedTot += clfFyr.NewMedTot
     C                   eval      clfEmp.NewRcd#   += clfFyr.NewRcd#
     C                   eval      clfEmp.NewRcdFee += clfFyr.NewRcdFee
     C                   eval      clfEmp.NewRcdTot += clfFyr.NewRcdTot
     C                   eval      clfEmp.New#      += clfFyr.New#
     C                   eval      clfEmp.NewTot    += clfFyr.NewTot
     C*
     C                   eval      clfEmp.ChgIM#      += clfFyr.ChgIM#
     C                   eval      clfEmp.ChgIMPrvFee += clfFyr.ChgIMPrvFee
     C                   eval      clfEmp.ChgIMCurFee += clfFyr.ChgIMCurFee
     C                   eval      clfEmp.ChgIMAdj    += clfFyr.ChgIMAdj
     C                   eval      clfEmp.ChgIR#      += clfFyr.ChgIR#
     C                   eval      clfEmp.ChgIRPrvFee += clfFyr.ChgIRPrvFee
     C                   eval      clfEmp.ChgIRCurFee += clfFyr.ChgIRCurFee
     C                   eval      clfEmp.ChgIRAdj    += clfFyr.ChgIRAdj
     C                   eval      clfEmp.ChgMI#      += clfFyr.ChgMI#
     C                   eval      clfEmp.ChgMIPrvFee += clfFyr.ChgMIPrvFee
     C                   eval      clfEmp.ChgMICurFee += clfFyr.ChgMICurFee
     C                   eval      clfEmp.ChgMIAdj    += clfFyr.ChgMIAdj
     C                   eval      clfEmp.ChgMR#      += clfFyr.ChgMR#
     C                   eval      clfEmp.ChgMRPrvFee += clfFyr.ChgMRPrvFee
     C                   eval      clfEmp.ChgMRCurFee += clfFyr.ChgMRCurFee
     C                   eval      clfEmp.ChgMRAdj    += clfFyr.ChgMRAdj
     C                   eval      clfEmp.ChgRI#      += clfFyr.ChgRI#
     C                   eval      clfEmp.ChgRIPrvFee += clfFyr.ChgRIPrvFee
     C                   eval      clfEmp.ChgRICurFee += clfFyr.ChgRICurFee
     C                   eval      clfEmp.ChgRIAdj    += clfFyr.ChgRIAdj
     C                   eval      clfEmp.ChgRM#      += clfFyr.ChgRM#
     C                   eval      clfEmp.ChgRMPrvFee += clfFyr.ChgRMPrvFee
     C                   eval      clfEmp.ChgRMCurFee += clfFyr.ChgRMCurFee
     C                   eval      clfEmp.ChgRMAdj    += clfFyr.ChgRMAdj
     C                   eval      clfEmp.Chg#        += clfFyr.Chg#
     C                   eval      clfEmp.ChgPrv      += clfFyr.ChgPrv
     C                   eval      clfEmp.ChgCur      += clfFyr.ChgCur
     C                   eval      clfEmp.ChgAdj      += clfFyr.ChgAdj
     C*
     C                   eval      genEmp.PriPaid     += genFyr.PriPaid
     C                   eval      genEmp.CurPaid     += genFyr.CurPaid
     c*** begin chg ***  05/11/11  **********************************************
     C*********          eval      genEmp.ClmPdPct    += genFyr.ClmPdPct
     c                   if        genFyr.ClmPdPct <> 0
     C                   eval      genEmp.ClmPdPct    = genFyr.ClmPdPct
     c                   endif
     c*** end chg *****  05/11/11  **********************************************
     C                   eval      genEmp.PriBillP    += genFyr.PriBillP
     C                   eval      genEmp.CurBillP    += genFyr.CurBillP
     C                   eval      genEmp.BilledP     += genFyr.BilledP
     C                   eval      genEmp.PriInc      += genFyr.PriInc
     C                   eval      genEmp.CurInc      += genFyr.CurInc
     c*** begin chg ***  05/11/11  **********************************************
     C*********          eval      genEmp.ClmIncPct   += genFyr.ClmIncPct
     c                   if        genFyr.ClmIncPct <> 0
     C                   eval      genEmp.ClmIncPct   = genFyr.ClmIncPct
     c                   endif
     c*** end chg *****  05/11/11  **********************************************
     C                   eval      genEmp.PriBillI    += genFyr.PriBillI
     C                   eval      genEmp.CurBillI    += genFyr.CurBillI
     C                   eval      genEmp.BilledI     += genFyr.BilledI
     C*
     C                   clear                   clfFyr
     C                   clear                   genFyr
     C*
     C                   exsr      GetEmpInfo
     C*
     C                   eval      *inof = *on
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process group breaks
     C*====================================================================
     C     BrkGrp        begsr
     C*
     C                   exsr      BrkEmp
     C*
     C                   eval      prtEmpHdg = *off
     C*
     C                   eval      totalLvl = 'GRP'
     C                   exsr      PrtAllTotals
     C*
     C                   eval      prtEmpHdg = *on
     C*
     C                   if        %open(qsysprt2)
     C                   close     qsysprt2
     C                   exsr      Dltqsysprt2
     C                   open      qsysprt2
     C                   endif
     C*
     C                   eval      clfTot.NewInd#   += clfGrp.NewInd#
     C                   eval      clfTot.NewIndFee += clfGrp.NewIndFee
     C                   eval      clfTot.NewIndTot += clfGrp.NewIndTot
     C                   eval      clfTot.NewMed#   += clfGrp.NewMed#
     C                   eval      clfTot.NewMedFee += clfGrp.NewMedFee
     C                   eval      clfTot.NewMedTot += clfGrp.NewMedTot
     C                   eval      clfTot.NewRcd#   += clfGrp.NewRcd#
     C                   eval      clfTot.NewRcdFee += clfGrp.NewRcdFee
     C                   eval      clfTot.NewRcdTot += clfGrp.NewRcdTot
     C                   eval      clfTot.New#      += clfGrp.New#
     C                   eval      clfTot.NewTot    += clfGrp.NewTot
     C*
     C                   eval      clfTot.ChgIM#      += clfGrp.ChgIM#
     C                   eval      clfTot.ChgIMPrvFee += clfGrp.ChgIMPrvFee
     C                   eval      clfTot.ChgIMCurFee += clfGrp.ChgIMCurFee
     C                   eval      clfTot.ChgIMAdj    += clfGrp.ChgIMAdj
     C                   eval      clfTot.ChgIR#      += clfGrp.ChgIR#
     C                   eval      clfTot.ChgIRPrvFee += clfGrp.ChgIRPrvFee
     C                   eval      clfTot.ChgIRCurFee += clfGrp.ChgIRCurFee
     C                   eval      clfTot.ChgIRAdj    += clfGrp.ChgIRAdj
     C                   eval      clfTot.ChgMI#      += clfGrp.ChgMI#
     C                   eval      clfTot.ChgMIPrvFee += clfGrp.ChgMIPrvFee
     C                   eval      clfTot.ChgMICurFee += clfGrp.ChgMICurFee
     C                   eval      clfTot.ChgMIAdj    += clfGrp.ChgMIAdj
     C                   eval      clfTot.ChgMR#      += clfGrp.ChgMR#
     C                   eval      clfTot.ChgMRPrvFee += clfGrp.ChgMRPrvFee
     C                   eval      clfTot.ChgMRCurFee += clfGrp.ChgMRCurFee
     C                   eval      clfTot.ChgMRAdj    += clfGrp.ChgMRAdj
     C                   eval      clfTot.ChgRI#      += clfGrp.ChgRI#
     C                   eval      clfTot.ChgRIPrvFee += clfGrp.ChgRIPrvFee
     C                   eval      clfTot.ChgRICurFee += clfGrp.ChgRICurFee
     C                   eval      clfTot.ChgRIAdj    += clfGrp.ChgRIAdj
     C                   eval      clfTot.ChgRM#      += clfGrp.ChgRM#
     C                   eval      clfTot.ChgRMPrvFee += clfGrp.ChgRMPrvFee
     C                   eval      clfTot.ChgRMCurFee += clfGrp.ChgRMCurFee
     C                   eval      clfTot.ChgRMAdj    += clfGrp.ChgRMAdj
     C                   eval      clfTot.Chg#        += clfGrp.Chg#
     C                   eval      clfTot.ChgPrv      += clfGrp.ChgPrv
     C                   eval      clfTot.ChgCur      += clfGrp.ChgCur
     C                   eval      clfTot.ChgAdj      += clfGrp.ChgAdj
     C*
     C                   select
     C                   when      saveFDTYPE = 'CLF' and saveFDBT = 'C'
     C                   eval      clfTotC = clfTot
     C                   when      saveFDTYPE = 'CLF' and saveFDBT = 'F'
     C                   eval      clfTotF = clfTot
     C                   when      saveFDTYPE = 'CLF' and saveFDBT = 'I'
     C                   eval      clfTotI = clfTot
     C                   endsl
     C*
     C                   eval      genTot.PriPaid     += genGrp.PriPaid
     C                   eval      genTot.CurPaid     += genGrp.CurPaid
     c*** begin chg ***  05/11/11  **********************************************
     C*********          eval      genTot.ClmPdPct    += genGrp.ClmPdPct
     c                   if        genGrp.ClmPdPct <> 0
     C                   eval      genTot.ClmPdPct    = genGrp.ClmPdPct
     c                   endif
     c*** end chg *****  05/11/11  **********************************************
     C                   eval      genTot.PriBillP    += genGrp.PriBillP
     C                   eval      genTot.CurBillP    += genGrp.CurBillP
     C                   eval      genTot.BilledP     += genGrp.BilledP
     C                   eval      genTot.PriInc      += genGrp.PriInc
     C                   eval      genTot.CurInc      += genGrp.CurInc
     c*** begin chg ***  05/11/11  **********************************************
     C*********          eval      genTot.ClmIncPct   += genGrp.ClmIncPct
     c                   if        genGrp.ClmIncPct <> 0
     C                   eval      genTot.ClmIncPct   = genGrp.ClmIncPct
     c                   endif
     c*** end chg *****  05/11/11  **********************************************
     C                   eval      genTot.PriBillI    += genGrp.PriBillI
     C                   eval      genTot.CurBillI    += genGrp.CurBillI
     C                   eval      genTot.BilledI     += genGrp.BilledI
     C*
     C                   clear                   clfGrp
     C                   clear                   genGrp
     C*
     C                   eval      *inof = *on
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process report layout breaks
     C*====================================================================
     C     BrkLayout     begsr
     C*
     C                   exsr      BrkFyr
     C*
     C                   clear                   clfGrp
     C                   clear                   clfTot
     C*
     C                   eval      *inof = *on
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process claim status breaks
     C*====================================================================
     C     BrkStatus     begsr
     C*
     C                   exsr      BrkCase
     C*
     C                   select
     C                   when      FDTYPE = 'CLF'
     C                   exsr      LoadClmSts
     C                   eval      *inof = *on
     C                   eval      subHead = *on
     C                   exsr      ChkOverflow
     C                   endsl
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Calculate Claim Fee amounts - New
     C*====================================================================
     C     CalcCLFNew    begsr
     C*
     C                   select
     C                   when      FDCUTY = 'I'
     C                   eval      clfFyr.NewInd#   += 1
     C                   eval      clfFyr.NewIndFee += FDCUBL
     C                   eval      clfFyr.NewIndTot += FDCUBL
     C                   when      FDCUTY = 'M'
     C                   eval      clfFyr.NewMed#   += 1
     C                   eval      clfFyr.NewMedFee += FDCUBL
     C                   eval      clfFyr.NewMedTot += FDCUBL
     C                   when      FDCUTY = 'R'
     C                   eval      clfFyr.NewRcd#   += 1
     C                   eval      clfFyr.NewRcdFee += FDCUBL
     C                   eval      clfFyr.NewRcdTot += FDCUBL
     C                   endsl
     C*
     C                   eval      clfFyr.New#   += 1
     C                   eval      clfFyr.NewTot += FDCUBL
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Calculate Claim Fee amounts - Changed
     C*====================================================================
     C     CalcCLFChg    begsr
     C*
     C                   select
     C                   when      FDPRTY = 'I' and FDCUTY = 'M'
     C                   eval      clfFyr.ChgIM# += 1
     C                   eval      clfFyr.ChgIMPrvFee += FDPRBL
     C                   eval      clfFyr.ChgIMCurFee += FDCUBL
     C                   eval      clfFyr.ChgIMAdj += FDCUBL - FDPRBL
     C                   when      FDPRTY = 'I' and FDCUTY = 'R'
     C                   eval      clfFyr.ChgIR# += 1
     C                   eval      clfFyr.ChgIRPrvFee += FDPRBL
     C                   eval      clfFyr.ChgIRCurFee += FDCUBL
     C                   eval      clfFyr.ChgIRAdj += FDCUBL - FDPRBL
     C                   when      FDPRTY = 'M' and FDCUTY = 'I'
     C                   eval      clfFyr.ChgMI# += 1
     C                   eval      clfFyr.ChgMIPrvFee += FDPRBL
     C                   eval      clfFyr.ChgMICurFee += FDCUBL
     C                   eval      clfFyr.ChgMIAdj += FDCUBL - FDPRBL
     C                   when      FDPRTY = 'M' and FDCUTY = 'R'
     C                   eval      clfFyr.ChgMR# += 1
     C                   eval      clfFyr.ChgMRPrvFee += FDPRBL
     C                   eval      clfFyr.ChgMRCurFee += FDCUBL
     C                   eval      clfFyr.ChgMRAdj += FDCUBL - FDPRBL
     C                   when      FDPRTY = 'R' and FDCUTY = 'I'
     C                   eval      clfFyr.ChgRI# += 1
     C                   eval      clfFyr.ChgRIPrvFee += FDPRBL
     C                   eval      clfFyr.ChgRICurFee += FDCUBL
     C                   eval      clfFyr.ChgRIAdj += FDCUBL - FDPRBL
     C                   when      FDPRTY = 'R' and FDCUTY = 'M'
     C                   eval      clfFyr.ChgRM# += 1
     C                   eval      clfFyr.ChgRMPrvFee += FDPRBL
     C                   eval      clfFyr.ChgRMCurFee += FDCUBL
     C                   eval      clfFyr.ChgRMAdj += FDCUBL - FDPRBL
     C                   endsl
     C*
     C                   eval      clfFyr.Chg# += 1
     C                   eval      clfFyr.ChgPrv += FDPRBL
     C                   eval      clfFyr.ChgCur += FDCUBL
     C                   eval      clfFyr.ChgAdj += FDCUBL - FDPRBL
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Calculate General amounts
     C*====================================================================
     C     CalcGenI      begsr
     C*
     C                   eval      genFyr.PriInc    += 0
     C                   eval      genFyr.CurInc    += FDTINC
     c*** begin chg ***  05/11/11  **********************************************
     C*********          eval      genFyr.ClmIncPct += FDCPCT
     c                   if        FDCPCT <> 0
     C                   eval      genFyr.ClmIncPct = FDCPCT
     c                   endif
     c*** end chg *****  05/11/11  **********************************************
     C                   eval      genFyr.PriBillI  += FDPRBL
     C                   eval      genFyr.CurBillI  += FDCUBL
     C                   eval      genFyr.BilledI   += FDCUBL
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Calculate General amounts
     C*====================================================================
     C     CalcGenP      begsr
     C*
     C                   eval      genFyr.PriPaid  += 0
     C                   eval      genFyr.CurPaid  += FDTPD
     c*** begin chg ***  05/11/11  **********************************************
     C*********          eval      genFyr.ClmPdPct += FDCPCT
     c                   if        FDCPCT <> 0
     C                   eval      genFyr.ClmPdPct = FDCPCT
     c                   endif
     c*** end chg *****  05/11/11  **********************************************
     C                   eval      genFyr.PriBillP += FDPRBL
     C                   eval      genFyr.CurBillP += FDCUBL
     C                   eval      genFyr.BilledP  += FDCUBL
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Check overflow
     C*====================================================================
     C     ChkOverflow   begsr
     C*
     C                   if        *inof
     C                   exsr      LoadHdrInfo
     C                   exsr      PrtHeading
     C                   exsr      PrtHeadSub
     C                   endif
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Check for report breaks
     C*====================================================================
     C     ChkRptBrks    begsr
     C*
     C                   select
     C                   when      FDFND <> saveFDFND
     C                   exsr      BrkGrp
     C                   when      FDEMP# <> saveFDEMP#
     C                              or FDDIV <> saveFDDIV
     C                   exsr      BrkEmp
     C                   when      FDTYPE <> saveFDTYPE
     C                              or FDDESC <> saveFDDESC
     C                              or FDBT <> saveFDBT
     C                   exsr      BrkLayout
     C                   when      FDFYR <> saveFDFYR
     C                   exsr      BrkFyr
     C                   when      FDASTS <> saveFDASTS
     C                   exsr      BrkStatus
     C                   when      FDCASE <> saveFDCASE
     C                   exsr      BrkCase
     C                   endsl
     C*
     C                   exsr      UpdtLastRcd
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Write to DARTS for employer
     C*====================================================================
     C     Darts         begsr
     C*
     C                   if        %open(qsysprt2)
     C                   close     qsysprt2
     C                   endif
     C*
     C                   select
     C                   when      @@DlFg <> 'Y'
     C                   exsr      Dltqsysprt2
     C                   when      @@Drm <> *blank
     C                   exsr      WriteDARTS
     C                   other
     C                   exsr      Dltqsysprt2
     C                   endsl
     C*
     C                   if        not %open(qsysprt2)
     C                   open      qsysprt2
     C                   endif
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Delete qsysprt2 (already used or not needed)
     C*====================================================================
     C     Dltqsysprt2   begsr
     C*
     C                   if        dltQsys1
     C                   callp(e)  RunCmd('DLTSPLF FILE(qsysprt2) SPLNBR(*LAST)'
     C                                   :40)
     C                   endif
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Calculate and/or print final totals
     C*====================================================================
     C     FinalTotals   begsr
     C*
     C                   eval      prtGrpHdg = *off
     C                   eval      prtEmpHdg = *off
     C                   eval      totalLvl = 'TOT'
     C                   exsr      PrtAllTotals
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Get employer information
     C*====================================================================
     C     GetEmpInfo    begsr
     C*
     C* Get employer year information
     C                   eval      rrn = GetEmpYrRR(FDFYR:FDEMP#:FDDIV
     C                                             :FDFND:FDCO#)
     C                   clear                   ELPFDT
     C                   clear                   ELPTDT
     C     rrn           chain     wdelp
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Load Claim Status heading
     C*====================================================================
     C     LoadClmSts    begsr
     C*
     C                   select
     C                   when      saveFDASTS = 'N'
     C                   eval      xxNewOrChgd = '*** New Claims ***'
     C                   when      saveFDASTS = 'C'
     C                   eval      xxNewOrChgd = '*** Changed Claims ***'
     C                   endsl
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Load header information
     C*====================================================================
     C     LoadHdrInfo   begsr
     C*
     C                   eval      xxFeeDesc = GetFeeDesc(saveFDTYPE:'FEES')
     C                   eval      xxFeeDescHdg = %trim(xxFeeDesc)
     C                                        + ' Based on:  '
     C                                        + GetBillTpDesc(saveFDTYPE
     C                                                       :saveFDBT)
     C*
     C                   exsr      LoadClmSts
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Print totals
     C*====================================================================
     C     PrintTotals   begsr
     C*
     C                   select
     C                   when      saveFDTYPE = 'CLF' and saveFDBT = 'C'
     C                   eval      clfGrp = clfGrpC
     C                   eval      clfTot = clfTotC
     C                   when      saveFDTYPE = 'CLF' and saveFDBT = 'F'
     C                   eval      clfGrp = clfGrpF
     C                   eval      clfTot = clfTotF
     C                   when      saveFDTYPE = 'CLF' and saveFDBT = 'I'
     C                   eval      clfGrp = clfGrpI
     C                   eval      clfTot = clfTotI
     C                   endsl
     C*
     C                   select
     C                   when      totalLvl = 'FYR'
     C                   eval      clf = clfFyr
     C                   eval      gen = genFyr
     C                   when      totalLvl = 'GRP'
     C                   eval      clf = clfGrp
     C                   eval      gen = genGrp
     C                   when      totalLvl = 'TOT'
     C                   eval      clf = clfTot
     C                   eval      gen = genTot
     C                   endsl
     C*
     C                   exsr      ChkOverflow
     C*
     C                   select
     C                   when      saveFDTYPE = 'CLF'
     C                   clear                   total1Emp
     C                   if        prtEmpHdg
     C                   eval      total1Emp = ' - Employer '
     C                                       + %editc(saveFDEMP#:'Z') + ' '
     C                                       + %editc(saveFDDIV:'3') + ' '
     C                                       + %editc(saveFDFYR:'Z') + ' '
     C                   endif
     C                   eval      total1aHd = 'Totals for New Claims'
     C                                       + total1Emp
     C                   eval      total1bHd = 'Totals for Changed Claims'
     C                                       + total1Emp
     C                   except    total1a
     C                   except    total1b
     C                   when      saveFDTYPE = 'GEN' and saveFDBT = 'C'
     C                   exsr      PrintTotal2
     C                   when      saveFDTYPE = 'GEN' and saveFDBT = 'E'
     C* * *              except    total3a
     C                   except    total3b
     C                   when      saveFDTYPE = 'GEN' and saveFDBT = 'F'
     C* * *              except    total5a
     C* * *              except    total5b
     C                   when      saveFDTYPE = 'GEN' and saveFDBT = 'I'
     C                   exsr      PrintTotal2
     C                   when      saveFDTYPE = 'GEN' and saveFDBT = 'P'
     C                   except    total4a
     C* * *              except    total4b
     C                   endsl
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Print totals for report 2
     C*====================================================================
     C     PrintTotal2   begsr
     C*
     C                   eval      xxFrom = DtMDYY(ELPFDT)
     C                   eval      xxTo   = DtMDYY(ELPTDT)
     C                   select
     C                   when      saveFDBT = 'C'
     C                   except    total2p
     C                   when      saveFDBT = 'I'
     C                   except    total2i
     C                   endsl
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Print totals for all report types
     C*====================================================================
     C     PrtAllTotals  begsr
     C*
     C                   eval      xtTbId = 'FEES'
     C                   eval      xtKey  = *loval
     C     keyt1WTGTL    setll     wtgtl
     C                   dou       %eof(wtgtl)
     C     keytWTGTL     reade     wtgtl
     C                   if        not %eof(wtgtl)
     C                   eval      xtKey  = GTKEY
     C                   select
     C                   when      GTKEY = 'CLF'
     C                   eval      xfTbId = 'CFBB'
     C                   when      GTKEY = 'GEN'
     C                   eval      xfTbId = 'COBB'
     C                   endsl
     C     keyfWTGTL     setll     wtgtl
     C                   dou       %eof(wtgtl)
     C     keyfWTGTL     reade     wtgtl
     C                   if        not %eof(wtgtl)
     C                   eval      saveFDTYPE = xtKey
     C                   eval      saveFDBT   = GTKEY
     C                   if        prtFinalTtl
     C                   eval      subHead = *off
     C                   eval      *inof = *on
     C                   exsr      PrintTotals
     C                   endif
     C                   endif
     C                   enddo
     C     keyt1WTGTL    setgt     wtgtl
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Print report heading
     C*====================================================================
     C     PrtHeading    begsr
     C*
     C                   except    head
     C                   except    heade
     C*
     C                   if        prtGrpHdg
     C                   except    headGrp
     C                   endif
     C*
     C                   except    heada
     C*
     C                   if        prtEmpHdg
     C                   select
     C                   when      saveFDTYPE = 'GEN'
     C                              and (saveFDBT = 'E'
     C                                    or saveFDBT = 'F'
     C                                    or saveFDBT = 'P')
     C                   except    headEmp
     C                   except    headCap
     C                   other
     C                   except    headEmpY
     C                   endsl
     C                   endif
     C*
     C                   except    headb
     C*
     C                   if        prtEmpHdg
     C                   except    headMin
     C                   endif
     C*
     C                   eval      *inof = *off
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Print report heading - sub
     C*====================================================================
     C     PrtHeadSub    begsr
     C*
     C                   if        subHead and not firstRcd
     C                   select
     C                   when      saveFDTYPE = 'CLF'
     C                   except    head1
     C                   when      saveFDTYPE = 'GEN' and saveFDBT = 'C'
     C                   except    head2
     C                   when      saveFDTYPE = 'GEN' and saveFDBT = 'E'
     C                   except    head3
     C                   when      saveFDTYPE = 'GEN' and saveFDBT = 'F'
     C                   except    head5
     C                   when      saveFDTYPE = 'GEN' and saveFDBT = 'I'
     C                   except    head2
     C                   when      saveFDTYPE = 'GEN' and saveFDBT = 'P'
     C                   except    head4
     C                   endsl
     C                   endif
     C*
     C                   eval      subHead = *on
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process Report for Claim Fee
     C*====================================================================
     C     ProcessClf    begsr
     C*
     C                   if        subHead
     C* * *              except    head1
     C                   eval      subHead = *off
     C                   endif
     C*
     C                   except    detail1
     C*
     C                   select
     C                   when      FDASTS = 'N'
     C                   exsr      CalcCLFNew
     C                   when      FDASTS = 'C'
     C                   exsr      CalcCLFChg
     C                   endsl
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process report for % of Claims Paid - General Fee
     C*====================================================================
     C     ProcessGenC   begsr
     C*
     C                   exsr      ProcessRpt2
     C                   exsr      CalcGenP
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process report for % of Premium - General Fee
     C*====================================================================
     C     ProcessGenE   begsr
     C*
     C                   if        subHead
     C* * *              except    head3
     C                   eval      subHead = *off
     C                   endif
     C*
     C                   eval      TotBill3 = FDPRBL + FDCUBL
     C*
     C                   except    detail3
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process report for Flat Fee - General Fee
     C*====================================================================
     C     ProcessGenF   begsr
     C*
     C                   if        subHead
     C* * *              except    head5
     C                   eval      subHead = *off
     C                   endif
     C*
     C                   eval      TotBill5 = FDPRBL + FDCUBL
     C*
     C                   except    detail5
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process report for % of Claims Incurred - General Fee
     C*====================================================================
     C     ProcessGenI   begsr
     C*
     C                   exsr      ProcessRpt2
     C                   exsr      CalcGenI
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process report for % of Payroll - General Fee
     C*====================================================================
     C     ProcessGenP   begsr
     C*
     C                   if        subHead
     C* * *              except    head4
     C                   eval      subHead = *off
     C                   endif
     C*
     C                   eval      TotBill4 = FDPRBL + FDCUBL
     C*
     C                   except    detail4
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process each record
     C*====================================================================
     C     ProcessRcds   begsr
     C*
     C                   if        firstRcd
     C                   exsr      GetEmpInfo
     C                   exsr      UpdtLastRcd
     C                   exsr      LoadHdrInfo
     C                   eval      *inof = *on
     C                   endif
     C*
     C                   exsr      ChkRptBrks
     C                   eval      subHead = *on
     C                   exsr      ChkOverflow
     C*
      *** beg add **** 05/20/11 **********************************************
      /free
          select;
          when @@SSF = 'N';
           eval xxSS# = '*********';
          when @@SSF = '4';
           eval xxSS# = '*****' + %subst(fdss#:6:4);
          other;
       //***  Begin Chg  ***  06/09/20  **************
       //  eval      xxSS# = %subst(FDSS#:1:3) + '-'
       //                    + %subst(FDSS#:4:2) + '-'
       //                    + %subst(FDSS#:6:4);
           xxSS# = DisplaySS#(MaskData('SSN':'FDSS#':FDSS#));
       //***  End Chg  ***  06/09/20  **************
          endsl;
      /end-free
     C******             eval      xxSS# = %subst(FDSS#:1:3) + '-'
     C******                             + %subst(FDSS#:4:2) + '-'
     C******                             + %subst(FDSS#:6:4)
      *** end add **** 05/20/11 **********************************************
     C*
     C                   select
     C                   when      FDTYPE = 'CLF'
     C                   exsr      ProcessClf
     C                   when      FDTYPE = 'GEN' and FDBT = 'C'
     C                   exsr      ProcessGenC
     C                   when      FDTYPE = 'GEN' and FDBT = 'E'
     C                   exsr      ProcessGenE
     C                   when      FDTYPE = 'GEN' and FDBT = 'F'
     C                   exsr      ProcessGenF
     C                   when      FDTYPE = 'GEN' and FDBT = 'I'
     C                   exsr      ProcessGenI
     C                   when      FDTYPE = 'GEN' and FDBT = 'P'
     C                   exsr      ProcessGenP
     C                   endsl
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process report 2
     C*====================================================================
     C     ProcessRpt2   begsr
     C*
     C                   if        subHead
     C* * *              except    head2
     C                   eval      subHead = *off
     C                   endif
     C*
     c*** begin chg ***  08/30/11  ***************************************
     C*********          except    detail2
     c                   if        fdcase <> 0
     C                   except    detail2
     c                   endif
     c*** end chg *****  08/30/11  ***************************************
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Update last record hold values
     C*====================================================================
     C     UpdtLastRcd   begsr
     C*
     C                   eval      saveFDFND  = FDFND
     C                   eval      saveFDFNAM = FDFNAM
     C                   eval      saveFDEMP# = FDEMP#
     C                   eval      saveFDDIV  = FDDIV
     C                   eval      saveFDENAM = FDENAM
     C                   eval      saveFDTYPE = FDTYPE
     C                   eval      saveFDDESC = FDDESC
     C                   eval      saveFDBT   = FDBT
     C                   eval      saveFDFYR  = FDFYR
     C                   eval      saveFDASTS = FDASTS
     C                   eval      saveFDCASE = FDCASE
     C                   eval      saveFDASOF = FDASOF
     C                   eval      saveFDIVDT = FDIVDT
     C                   eval      saveFDCAP# = FDCAP#
     C                   eval      saveFDMINF = FDMINF
     C*
     C                   eval      firstRcd = *off
     C*
     C                   endsr
      *
      *====================================================================
      * PgmStr - Set-up and Initialize
      *====================================================================
     C     PgmStr        BEGSR
      *
     c                   if        @@PFB <> 'F'
     c                   open      qsysprt
     c                   open      qsysprt2
     c                   endif
     C*
     C                   eval      sysDate = SystemDate
     C                   eval      sysTime = SystemTime
      *
     C* Get copyright information
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
      *
     C                   ENDSR                                                  PgmStr
      *

      *====================================================================
      * Print routing page
      *====================================================================
     C     Routing       BEGSR
      *
     C                   select
     C                   when      kkEmp# <> *zero
     C                   eval      routeEmp = %trim(%editc(kkEmp#:'X'))
     C                                      + ' '
     C                                      + %trim(%editc(kkDiv:'X'))
     C                   other
     C                   eval      routeEmp = '*ALL'
     C                   endsl
      *
      *   Output Routing - detail report 0
      *
     C                   MOVE      '0'           KKPRT
     C*
     C                   eval      upUsNm = GetUsrName(q1User)
     C                   eval      ORTITL = title0
     C                   eval      ORDIS1 = upUsNm
     C     keySMORL      chain     smorl
     C                   eval      ORTITL = LeftJstify(%trim(ORTITL))
      *
     C                   eval      xxQue = xxOutQ
     C     xxQue         comp      *blanks                            090908
      *
     C                   if        %found(SMORL)
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         except    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   endif
      *
     C                   eval      ORTITL = CenterFld(ORTITL)
      *
      * Center Company name in display field
     C                   eval      xxCoName = CenterFld(GetCompNam(kkCo#))
      *
     C                   endsr                                                  Routing
     C*
     C*====================================================================
     C* Write DARTS remark
     C*====================================================================
     C     WriteDARTS    begsr
      *
     C                   eval      ##Co# = %editc(FDCO#:'X')
     C                   eval      ##Fnd = %editc(saveFDFND:'X')
     C                   call      'SNXTDCID'
     C                   parm      *BLANKS       ##Flr
     C                   parm      *BLANKS       ##DcId
     C                   parm      'FE'          ##PRE             2
     C                   parm                    ##CO#             3
     C                   parm                    ##FND             3
     C                   parm      'P'           ##RTCD            1
     C*
     C                   call      'SRESOLVE'
     C                   parm      'qsysprt2'    ##SplF           10
     C                   parm                    ##Flr
     C                   parm                    ##DcId
     C                   parm      *blanks       ##OutQ           10
     C*
     C                   eval      ##Key = %editc(FDCO#:'X')
     C                                   + %editc(saveFDFND:'X')
     C                                   + %editc(saveFDEMP#:'X')
     C                                   + %editc(saveFDDIV:'X')
     C                   eval      ##Comm = 'Fee Invoice Detail Report'
     c                   eval      ##CDat = %editc(DateTo8(sysDate):'X')
     C*
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01
     c*
     c     d1ky          setgt     pcm02
     c     d1ky          readpe    pcm02                                  60
     c*
     c                   if        syscif = 'Y' and *in60 = *off
     c
     C                   MOVEL(P)  ##DCID        Q#DCID           12
     C                   MOVEL(P)  ##FLR         Q#FOLD           60
     c                   movel     c2cmseq#      q#seq#            5
     c                   move      'O'           q#otyp            1
     c                   movel     ##comm        q#odsc           40
     c                   move      @@DRM         q#rmcd            5
     c                   movel     'AS400'       q#sysn           20
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVEL(P)  Q1USER        Q#USER           10
     c
     c                   call      'ISEWMD'
     C                   PARM                    ##KEY
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#DCID
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#OTYP
     C                   PARM                    Q#ODSC
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SYSN
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#USER
     C                   PARM      ' '           QQRTCD            1
     C                   PARM      *blanks       QQEDSC           80
     c*
     c                   else
     C*
     C                   call      'DA700R'
     C                   parm                    ##Key            40
     C                   parm                    ##Comm          256
     C                   parm                    ##CDat            8
     C                   parm                    ##DcId           12
     C                   parm      'W0050R'      ##PNAM           10
     C                   parm      *zeros        ##SECL            3
     C                   parm      q1user        ##USER           10
     C                   parm      q1pgm         ##PGM            10
     C                   parm                    ##Flr            80
     C                   parm      'N'           ##DIFL            1
     C                   parm      q1pgm         ##IPGM           10
     C                   parm      *blank        ##FL01            1
     C                   parm      *blank        ##FL02            1
     C                   parm      @@drm         ##RMCD            5
     C                   parm      *zeros        ##SE##            5
     C                   parm      'N'           ##SBYL            1
     C                   parm      *blanks       ##COM2           19
     C                   parm      *blanks       ##APID            2
     C                   parm      *blanks       ##IDOC           12
     C                   parm      *blanks       ##IQUE           10
     C                   parm      'O'           ##IMGF            1
     C                   parm      *blanks       ##IPTH           10
     C                   parm      *zeros        ##PIDX            5
     C                   parm      *blanks       ##QPTY            1
     C                   parm      *blanks       ##DCTP            5
     C                   parm      *blanks       ##CSID            2
     C                   parm      *blanks       ##CODE            2
     c
     c                   endif                                                  if syscif = 'Y'
     C*
     C                   exsr      Dltqsysprt2
     C*
     C                   endsr
      *
     O/copy SCOPYROUT
      *
     O          E            ROUTE          2 33
     O                                           24 'REPORT SELECTIONS'
      *
     O          E            ROUTE          2
     O                                         +010 'COMPANY . . . . . . . . :'
     O                       kkCO#         Z   +001
      *
     O          E            ROUTE          2
     O                                         +010 'EMPLOYER  . . . . . . . :'
     O                       routeEmp          +001
      *
     O          E            ROUTE          2
     O                                         +010 'POSTING DATE  . . . . . :'
     O                       x8podt            +001 '  /  /    '
      *
     O          E            ROUTE          2
     O                                         +010 'PRINT DATE  . . . . . . :'
     O                       x8prdt            +001 '  /  /    '
      *
     O          E            ROUTE          2
     O                                         +010 'INVOICES AS OF DATE . . :'
     O                       x8LsDt            +001 '  /  /    '
      *
     O          E            ROUTE          2
     O                                         +010 'DARTS REMARK FLAG . . . :'
     O                       @@dlfg            +001
      *
     O          E            ROUTE          2
     O                                         +010 'DARTS REMARK  . . . . . :'
     O                       @@drm             +001
      *
     O          E            ROUTE          2
     O                                         +010 'PRINT SS# . . . .(Y/4/N):'
     O                       @@SSF             +001
      *
     O          E            ROUTE          2
     O                                         +010 'EXCLUDE POLICIES WITH'
     O                                         +001 'NO ACTIVITY . . . . . . :'
     O                       @@excl            +001
      *
     O          E            ROUTE          2
     O                                         +010 'OUTPUT (Print/File/Both):'
     O                       @@pfb             +001
      *
      *
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
      *
      /copy W8006OH
      /copy W8006OHDG
      /copy W8006O1
      /copy W8006O2
      /copy W8006O3
      /copy W8006O4
      /copy W8006O5
     Oqsysprt2  E
      /copy W8006OHE
      /copy W8006OHDG
      /copy W8006O1
      /copy W8006O2
      /copy W8006O3
      /copy W8006O4
      /copy W8006O5





     P*====================================================================
     P* Get the bill type description
     P*====================================================================
     PGetBillTpDesc    b
     DGetBillTpDesc    pi                  like(GTDESC)
     D type                                like(FDTYPE) const
     D key                                 like(GTKEY)  const

     D tableId         s                   like(GTTBID) inz

     C     keyWTGTL      klist
     C                   kfld                    kkCo#
     C                   kfld                    tableId
     C                   kfld                    key

     C                   select
     C                   when      type = 'CLF'
     C                   eval      tableId = 'CFBB'
     C                   when      type = 'GEN'
     C                   eval      tableId = 'COBB'
     C                   endsl
     C*
     C                   clear                   GTDESC
     C     keyWTGTL      chain     wtgtl
     C                   return    GTDESC

     PGetBillTpDesc    e





     P*====================================================================
     P* Get the fee description
     P*====================================================================
     PGetFeeDesc       b
     DGetFeeDesc       pi                  like(GTDESC)
     D key                                 like(GTKEY)  const
     D tableId                             like(GTTBID) const

     C     keyWTGTL      klist
     C                   kfld                    kkCo#
     C                   kfld                    tableId
     C                   kfld                    key

     C                   clear                   GTDESC
     C     keyWTGTL      chain     wtgtl
     C                   return    GTDESC

     PGetFeeDesc       e
