     h/title FixNetDed - Fix Net Deductibles
     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
      *=================================================================================
      *  This program searches for and reverses excess net deductible payment
      *  recoveries.  It loops through WDELP and for each net deductible policy:
      *   (1) Retrieves group info
      *   (2) Retrieves employer info
      *   (3) Checks to make sure the application net deductible process actually
      *        processed on the policy.
      *   (4) Loops through each claim on the policy. For each claim encountered it
      *       calculates for excess recovery by subtracting total deductible billed
      *        billed from total paid recoveries for each type. If excess exists:
      *       (a) Write a negative net deductible payment to the claim payment file for
      *            the reserve bucket being processed.
      *       (b) Write the check register for the negative payment.
      *       (c) Write the negative request refund to the the claims recovery request file.
      *       (d) Adjust amount recovered and clear amount pending in all deductible billing
      *            for the case being processed in the deductible detail file.
      *       (e) Add to the paid bucket in Claims Master. If open, subtract from the
      *            remaining reserve. If closed, write a Reserve History change.
      *       (f) Write a post net record with check# 8888888 to allow redistribution
      *           via the post net recovery process.
      *       (g) Adjust total claims paid and outstanding reserve in Employer Detail.
      *=================================================================================
     fwmupl     if   e           k disk    rename(pup01:pup0l)
     fwmfnl     if   e           k disk    rename(pfn01:pfn0l)
     fwmeml     if   e           k disk    rename(pem01:pem0l)                  co/fnd/emp/div
     fwdpal3    if   e           k disk    rename(ppa01:ppa0l3)                 co/fnd/fyr/case/pv1
     fwtddl2    if   e           k disk    rename(pdd01:pdd0lt)                 co/fnd/st/code/efdt
     fwtabl     if   e           k disk    include(pb201)
     fwdell     uf   e           k disk    rename(pel01:pel0l)                  co/fnd/emp/div/fyr
     fwmcml     uf   e           k disk    rename(pcm01:pcm0l)                  co/fnd/emp/div/fyr
     fwdddl1    uf   e           k disk    rename(pdd01:pdd0l1)                 co/fnd/emp/div/fyr/c
     fwmrhp     if a e           k disk    rename(prh01:prh0p)
     fwdpal     if a e           k disk    rename(ppa01:ppa0l)                  co/fnd/fyr/case/seq#
     fwdrpp     if a e           k disk    rename(prp01:prp0p)
     fwmrrl     if a e           k disk    rename(prr01:prr0l)
     fwadql     uf a e           k disk    rename(pdq01:pdq0l)
     d psds          esds                  extname(szq1p)
      *==============================================================
      * Subprocedure Prototypes
      *==============================================================
      /copy WprEmplr
      /copy SprDate
     d PopNetDedRsv    pr
     d GetBilling      pr            +2    like(DDAMTB)
     d  inReserveCode                 2    const
     d GetRcvrPays     pr            +2    like(PAAMT)
     d  inNetDedCode                  2    const
     d QryIfNet        pr              n
     d  inDedCode                     3    const
     d ReversRcvry     pr
     d  inReserveCode                 2    const
     d  inNetDedCode                  2    const
     d  inRecoveryAmt                +2    like(PAAMT) const
     d AppWrtRcvry     pr              n
     d  inNetDedCode                  2    const
     d WriteNegNetPay  pr
     d  inNetDedCode                  2    const
     d  inRecoveryAmt                +2    like(PAAMT) const
     d WriteChkRegstr  pr
     d  inReserveCode                 2    const
     d  inRecoveryAmt                +2    like(PAAMT) const
     d WriteReqRefund  pr
     d  inRecoveryAmt                +2    like(PAAMT) const
     d WritePostNet    pr
     d  inRecoveryAmt                +2    like(PAAMT) const
     d AdjustWDDDP     pr
     d  inReserveCode                 2    const
     d AdjustWMCMP     pr
     d  inReserveCode                 2    const
     d  inRecoveryAmt                +2    like(PAAMT) const
     d WrtRsvHistory   pr
     d  index                         2  0 const
     d  inRecoveryAmt                +2    like(PAAMT) const
     d UpdateWdelp     pr
      *==============================================================
      * Stand Alone Fields
      *==============================================================
     d idx             s              1  0
     d isNet           s               n
     d billTotal       s             +2    like(DDamtb)
     d rcvrTotal       s             +2    like(PAamt)
     d savFnd          s                   like(FNfnd)  inz(0)
     d savEmp#         s                   like(EMemp#) inz(0)
     d savDiv          s                   like(EMdiv)  inz(0)
     d polClmPdAdj     s                   like(ELtclp)
     d polClmRsvAdj    s                   like(ELouts)
     d #OfCodes        c                   const(4)
      *==============================================================
      * Data Structures/Arrays
      *==============================================================
     d netDedCodDS     ds
     d  FNmep1
     d  FNcop1
     d  FNlep1
     d  FNexp1
     d  netDedCodes                   2    Overlay(netDedCodDS) Dim(#OfCodes)

     d netDedRsvDS     ds
     d  MeRsvType                     2
     d  CoRsvType                     2
     d  LeRsvType                     2
     d  EpRsvType                     2
     d  netDedRsv                     2    Overlay(netDedRsvDS) Dim(#OfCodes)

     d rsvCodeDs       ds
     d  CMrt01
     d  CMrt02
     d  CMrt03
     d  CMrt04
     d  CMrt05
     d  CMrt06
     d  CMrt07
     d  CMrt08
     d  CMrt09
     d  CMrt10
     d  rsvCode                       2    Overlay(rsvCodeDs) Dim(10)

     d rsvPaidDs       ds
     d  CMar01
     d  CMar02
     d  CMar03
     d  CMar04
     d  CMar05
     d  CMar06
     d  CMar07
     d  CMar08
     d  CMar09
     d  CMar10
     d  rsvPaid                       9  2 Overlay(rsvPaidDs) Dim(10)

     d rsvRemainDs     ds
     d  CMrr01
     d  CMrr02
     d  CMrr03
     d  CMrr04
     d  CMrr05
     d  CMrr06
     d  CMrr07
     d  CMrr08
     d  CMrr09
     d  CMrr10
     d  rsvRemain                     9  2 Overlay(rsvRemainDs) Dim(10)

     d rsvHstCdDs      ds
     d  RHrt01
     d  RHrt02
     d  RHrt03
     d  RHrt04
     d  RHrt05
     d  RHrt06
     d  RHrt07
     d  RHrt08
     d  RHrt09
     d  RHrt10
     d  rsvHstCd                      2    Overlay(rsvHstCdDs) Dim(10)

     d rsvHstChgDs     ds
     d  RHsu01
     d  RHsu02
     d  RHsu03
     d  RHsu04
     d  RHsu05
     d  RHsu06
     d  RHsu07
     d  RHsu08
     d  RHsu09
     d  RHsu10
     d  rsvHstChg                     9  2 Overlay(rsvHstChgDs) Dim(10)
      *==============================================================
      * Main
      *==============================================================
      /free
       Chain Q1user pup0l;
       Setll UPco# pel0l;
       Reade(n) UPco# pel0l;
       Dow not(%eof);

          If savFnd <> ELfnd;
             Chain (ELco#:ELfnd) pfn0l;
             If not(%found);
                Setgt (ELco#:ELfnd) pel0l;
                Reade(n) (ELco#) pel0l;
                Iter;
             Endif;
             PopNetDedRsv();
          Endif;

          If savFnd <> ELfnd or savEmp# <> Elemp# or savDiv <> ELdiv;
             savFnd = ELfnd;
             savEmp# = ELemp#;
             savDiv  = ELdiv;
             Chain (ELco#:ELfnd:ELemp#:ELdiv) pem0l;
             If not(%found);
                Setgt (ELco#:ELfnd:ELemp#:ELdiv) pel0l;
                Reade(n) (ELco#) pel0l;
                Iter;
             Endif;
          Endif;

          Select;
          When ELdcd2 <> *blank;
             isNet = QryIfNet(ELdcd2);
          When ELdcde <> *blank;
             isNet = QryIfNet(Eldcde);
          Other;
             isNet = *off;
          Endsl;

          polClmPdAdj  = 0;
          polClmRsvAdj = 0;
          If isNet;
             Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pcm0l;
             Reade(n) (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pcm0l;
             Dow not(%eof);

                idx  = 1;
                Dow idx <= #OfCodes;
                   If netDedCodes(idx) = *blank;
                      idx += 1;
                      iter;
                   Endif;
                   If AppWrtRcvry(netDedCodes(idx)) = *off;
                      idx += 1;
                      iter;
                   Endif;
                   billTotal = GetBilling(netDedRsv(idx));
                   rcvrTotal = GetRcvrPays(netDedCodes(idx));
                   If rcvrTotal > billTotal;
                      polClmPdAdj += (rcvrTotal - billTotal);
                      ReversRcvry(netDedRsv(idx):netDedCodes(idx):
                       rcvrTotal - billTotal);
                   Endif;
                idx += 1;
                Enddo;

             Reade(n) (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pcm0l;
             Enddo;
          Endif;

          If polClmPdAdj > 0 and isNet;
             UpdateWdelp();
          Endif;

       Reade(n) UPco# pel0l;
       Enddo;

       *inlr = *on;
      /end-free
      *==============================================================
      * Subprocedure Interfaces
      *==============================================================
     p AppWrtRcvry     b
     d AppWrtRcvry     pi              n
     d  inNetDedCode                  2    const
      /free
       Setll (CMco#:CMfnd:CMfyr:CMcase:inNetDedCode) ppa0l3;
       Reade (CMco#:CMfnd:CMfyr:CMcase:inNetDedCode) ppa0l3;
       Dow not(%eof);
          If PApgm = 'W8021R1';
             return *on;
          Endif;
       Reade (CMco#:CMfnd:CMfyr:CMcase:inNetDedCode) ppa0l3;
       Enddo;
       Return *off;
      /end-free
     p AppWrtRcvry     e
      *==============================================================
     p PopNetDedRsv    b
     d idx             s              1  0
      /free
       idx = 1;
       Dow idx <= #OfCodes;
          Chain (UPco#:netDedCodes(idx)) pb201;
          If %found;
             netDedRsv(idx) = B2rc01;
          Endif;
       idx += 1;
       Enddo;
      /end-free
     p PopNetDedRsv    e
      *==============================================================
     p QryIfNet        b
     d QryIfNet        pi              n
     d  dedCode                       3    const
     d rateState       s              2
      /free
       rateState = GetRatingState(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
       Setgt (ELco#:ELfnd:rateState:dedCode:ELpfdt) pdd0lt;
       Reade (ELco#:ELfnd:rateState:dedCode) pdd0lt;
       Select;
       When %eof;
          Return *off;
       When DDtype = 'N';
          Return *on;
       Other;
          Return *off;
       Endsl;
      /end-free
     p QryIfNet        e
      *==============================================================
     p GetBilling      b
     d GetBilling      pi            +2    like(DDAMTB)
     d  inReserveCode                 2    const
     d  total          s             +2    like(DDAMTB) inz(0)
      /free
       Setll (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:Cmcase) pdd0l1;
       Reade (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:Cmcase) pdd0l1;
       Dow not(%eof);
          If DDrecd =inReserveCode;
             total += DDamtb;
          Endif;
       Reade (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:Cmcase) pdd0l1;
       Enddo;
       Return total;
      /end-free
     p GetBilling      e
      *==============================================================
     p GetRcvrPays     b
     d GetRcvrPays     pi            +2    like(PAAMT)
     d  inNetDedCode                  2    const
     d  payTotal       s             +2    like(DDAMTB) inz(0)
      /free
       Setll (CMco#:CMfnd:CMfyr:CMcase) ppa0l;
       Reade (CMco#:CMfnd:CMfyr:CMcase) ppa0l;
       Dow not(%eof);
          If PAbr2  = inNetDedCode;
             payTotal += PAamt;
          Endif;
       Reade (CMco#:CMfnd:CMfyr:CMcase) ppa0l;
       Enddo;
       Return payTotal;
      /end-free
     p GetRcvrPays     e
      *==============================================================
     p ReversRcvry     b
     d ReversRcvry     pi
     d  inReserveCode                 2    const
     d  inNetDedCode                  2    const
     d  inRecoveryAmt                +2    like(PAAMT) const
      /free
       WriteNegNetPay(inNetDedCode:inRecoveryAmt);
       WriteChkRegstr(inReserveCode:inRecoveryAmt);
       WriteReqRefund(inRecoveryAmt);
       WritePostNet(inRecoveryAmt);
       AdjustWDDDP(inReserveCode);
       AdjustWMCMP(inReserveCode:inRecoveryAmt);
       Return;
      /end-free
     p ReversRcvry     e
      *==============================================================
     p WriteNegNetPay  b
     d WriteNegNetPay  pi
     d  inNetDedCode                  2    const
     d  inRecoveryAmt                +2    like(PAamt) const
     d nextSeq#        s                   like(PAseq#)
      /free
       Setgt  (CMco#:CMfnd:CMfyr:CMcase) ppa0l;
       Readpe (CMco#:CMfnd:CMfyr:CMcase) ppa0l;
       nextSeq# = PAseq# + 1;
       Dow not(%eof);
          If PAbr2  =  inNetDedCode;
             PAchk# =  8413400;
             PAseq# =  nextSeq#;
             PAamt  = -inRecoveryAmt;
             PApgm  =  Q1pgm;
             PAuser =  Q1user;
             PAdt   =  Dateto7(SystemDate());
             PAtrdt =  PAdt;
             PApodt =  PAdt;
             PAtime =  Timeto6(SystemTime());
             PAipgm =  Q1pgm;
             PAiusr =  Q1user;
             PAidt  =  PAdt;
             PAtime =  PAtime;
             Write ppa0l;
             Leave;
          Endif;
       Readpe (CMco#:CMfnd:CMfyr:CMcase) ppa0l;
       Enddo;
       Return;
      /end-free
     p WriteNegNetPay  e
      *==============================================================
     p WriteChkRegstr  b
     d WriteChkRegstr  pi
     d  inReserveCode                 2    const
     d  inRecoveryAmt                +2    like(PAAMT) const
      /free
       Clear prp0p;
       RPco#  = PAco#;
       RPfnd  = PAfnd;
       RPfyr  = PAfyr;
       RPcase = PAcase;
       RPpnam = EMname;
       If EMmpfl = 'M';
          RPadr1 = EMmad1;
          RPadr2 = EMmad2;
          RPcty  = EMmcty;
          RPst   = EMmst;
          RPzip  = EMmzip;
       Else;
          RPadr1 = EMpad1;
          RPadr2 = EMpad2;
          RPcty  = EMpcty;
          RPst   = EMpst;
          RPzip  = EMpzip;
       Endif;
       RPtrdt = PAtrdt;
       RPseq# = PAseq#;
       RPuser = Q1user;
       RPpgm  = Q1pgm;
       RPdt   = Dateto7(SystemDate());
       RPtime = Timeto6(SystemTime());
       Write prp0p;
      /end-free
     p WriteChkRegstr  e
      *==============================================================
     p WriteReqRefund  b
     d WriteReqRefund  pi
     d  inRecoveryAmt                +2    like(PAAMT) const
      /free
       Setgt  (CMco#:CMfnd:CMfyr:CMcase) prr0l;
       Readpe (CMco#:CMfnd:CMfyr:CMcase) prr0l;
       If not(%eof);
          RRreq# = RRreq# + 1;
       Else;
          RRreq# = 1;
       Endif;
       RRco#  = CMco#;
       RRfnd  = CMfnd;
       RRfyr  = CMfyr;
       RRcase = CMcase;
       RRfein = 0;
       RRmod  = 0;
       RRdtrc = PAtrdt;
       RRrcvd = -inRecoveryAmt;
       RRramt = 0;
       RRdesc = PAdesc;
       RRuser = Q1user;
       RRpgm  = Q1pgm;
       RRdt   = Dateto7(SystemDate());
       RRtime = Timeto6(SystemTime());
       RRstat = 'A';
       Write prr0l;
      /end-free
     p WriteReqRefund  e
      *==============================================================
     p WritePostNet    b
     d WritePostNet    pi
     d  inRecoveryAmt                +2    like(PAAMT) const
     d chk8s           s                   like(PAamt) inz(8413400)
      /free
       Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:chk8s) pdq0l;
       DQco#  = CMco#;
       DQfnd  = CMfnd;
       DQfyr  = CMfyr;
       DQemp# = CMemp#;
       DQdiv  = CMdiv;
       DQchk# = chk8s;
       DQuser = Q1user;
       DQpgm  = Q1pgm;
       DQdt   = Dateto7(SystemDate());
       DQtime = Timeto6(SystemTime());
       If %found;
          DQamtr += inRecoveryAmt;
          DQamt  += inRecoveryAmt;
          Update pdq0l;
       Else;
          DQamtr = inRecoveryAmt;
          DQamt  = inRecoveryAmt;
          Write  pdq0l;
       Endif;
      /end-free
     p WritePostNet    e
      *==============================================================
     p AdjustWDDDP     b
     d AdjustWDDDP     pi
     d  inReserveCode                 2    const
      /free
       Setll (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:Cmcase) pdd0l1;
       Reade (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:Cmcase) pdd0l1;
       Dow not(%eof);
          If DDrecd = inReserveCode;
             DDamtr = DDamtb;
             DDamtp = 0;
             DDpgm  = Q1pgm;
             DDuser = Q1user;
             DDdt   = Dateto7(SystemDate());
             DDtime = Timeto6(SystemTime());
             Update pdd0l1;
          Endif;
       Reade (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:Cmcase) pdd0l1;
       Enddo;
      /end-free
     p AdjustWDDDP     e
      *==============================================================
     p AdjustWMCMP     b
     d AdjustWMCMP     pi
     d  inReserveCode                 2    const
     d  inRecoveryAmt                +2    like(PAAMT) const
     d idx             s              2  0
      /free
       Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:CMcase) pcm0l;
       idx = %LookUp(inReserveCode:rsvCode);
       rsvPaid(idx) += inRecoveryAmt;
       If CMstat = 'O';
          rsvRemain(idx) -= inRecoveryAmt;
          polClmRsvAdj   += inRecoveryAmt;
          If rsvRemain(idx) < 0;
             WrtRsvHistory(idx:-rsvRemain(idx));
             polClmRsvAdj += rsvRemain(idx);
             rsvRemain(idx) = 0;
          Endif;
       Else;
          WrtRsvHistory(idx:inRecoveryAmt);
       Endif;
       CMpgm  = Q1pgm;
       CMuser = Q1user;
       CMdt   = Dateto7(SystemDate());
       CMtime = Timeto6(SystemTime());
       Update pcm0l;
      /end-free
     p AdjustWMCMP     e
      *==============================================================
     p WrtRsvHistory   b
     d WrtRsvHistory   pi
     d  index                         2  0 const
     d  inRecoveryAmt                +2    like(PAAMT) const
      /free
       Clear prh0p;
       RHco#  = CMco#;
       RHfnd  = CMfnd;
       RHfyr  = CMfyr;
       RHcase = CMcase;
       rsvHstCd(index)  = rsvCode(index);
       rsvHstChg(index) = inRecoveryAmt;
       RHuser = Q1user;
       RHpgm  = Q1pgm;
       RHtime = Timeto6(SystemTime());
       RHdt   = Dateto7(SystemDate());
       RHefdt = RHdt;
       RHiusr = Q1user;
       RHipgm = Q1pgm;
       RHidt  = RHdt;
       RHitim = RHtime;
       Write prh0p;
      /end-free
     p WrtRsvHistory   e
      *==============================================================
     p UpdateWdelp     b
      /free
       Chain (Elco#:ELfnd:ELemp#:ELdiv:ELfyr) pel0l;
       ELtclp += polClmPdAdj;
       ELouts -= polClmRsvAdj;
       ELuser = Q1user;
       ELpgm  = Q1pgm;
       ELtime = Timeto6(SystemTime());
       ELdt   = Dateto7(SystemDate());
       Update pel0l;
      /end-free
     p UpdateWdelp     e
