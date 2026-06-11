     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/title REVPOSTNET - Reverse Post Net Recoveries
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
      *==========================================================
      * You can't call this pgm directly. You must call REVPOSTNEC.
      * The update flag controls whether production files are
      *  updated or not.
      * Accepts parms update/co/date/fnd/emp/div/fyr/case/chk# and
      *  reverses recoveries posted on that date. It reads
      *  through WDPAP and if a recovery record is found
      *  that matches the selection criteria it is deleted
      *  and that amount applied back to the deductible detail,
      *  employer detail, claims master, reserve history, and post net.
      *==========================================================
      * sort overridden to co/fnd/case by OPNQRYF in CL
     fwdpal     uf   e           k disk    rename(ppa01:ppa00)
      * co/fnd
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
      * co/fnd/fyr/case
     fwmcml1    uf   e           k disk
      * co/fnd/fyr/case/bfyr/podt/sort
     fwdddl4    uf   e           k disk    rename(pdd01:pdd04)
      * co/fnd/emp/div/fyr
     fwdell     uf   e           k disk    rename(pel01:pel00)
      * co/fnd/emp/div/fyr/chk#
     fwadql     uf   e           k disk    rename(pdq01:pdq00)
     fwmrhp     o    e             disk    rename(prh01:prh0p)
     frevposterro    e             disk

      * archive files
     fwdpap     o    e             disk    rename(ppa01:ppa0p)
     fwmcmp     o    e             disk    rename(pcm01:pcm0p)
      * co/fnd/emp/div/fyr/case
     fwmcml     if   e           k disk    rename(pcm01:pcm00)
     fwdddp     o    e             disk    rename(pdd01:pdd0p)
      * co/fnd/fyr/case/bfyr/podt/recd
     fwdddl     if   e           k disk    rename(pdd01:pdd00)
     fwdelp     o    e             disk    rename(pel01:pel0p)
      * co/fnd/fyr/emp/div
     fwdell1    uf   e           k disk
     fwadqp     o    e             disk    rename(pdq01:pdq0p)

     d*entry
     d RevPostNet      pr
     d  @@update                      1
     d  @@co#                         3
     d  @@trdt                        7
     d  @@fnd                         3
     d  @@case                        7
     d  @@emp#                        9
     d  @@div                         5
     d  @@fyr                         3
     d  @@chk#                        7
     d RevPostNet      pi
     d  @@update                      1
     d  @@co#                         3
     d  @@trdt                        7
     d  @@fnd                         3
     d  @@case                        7
     d  @@emp#                        9
     d  @@div                         5
     d  @@fyr                         3
     d  @@chk#                        7
      *prototypes
      /copy SprDate
      /copy wPrClmRsv
     d ArcWADQP        pr
     d ArcWDDDP        pr
     d ArcWDELP        pr
     d ArcWMCMP        pr
     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  qqPgmName                    10    const
     d  qqFound                       1
     d GetRsvCode      pr             2
     d  x                             2  0
     d ProcessPayment  pr              n
     d RevWDDDP        pr              n
     d  inAmt                              like(PAamt)
     d  inMode                        1    const
     d RevWMCMP        pr              n
     d  inAmt                              like(PAamt)
     d  inMode                        1    const
     d RevWADQP        pr              n
     d  inAmt                              like(PAamt)
     d  inMode                        1    const
     d RevWDELP        pr              n
     d  inAmt                              like(PAamt)
     d  inMode                        1    const
     d WriteError      pr
     d  inMode                        1    const
     d WriteWMRHP      pr
      *stand alone
     d qqFound         s              1
     d processRcd      s               n
     d xxTrDt          s                   like(PAtrdt)
     d xxCo#           s                   like(PAco#)
     d xxFnd           s                   like(PAfnd)
     d svFnd           s                   like(PAfnd)
     d xxEmp#          s                   like(CMemp#)
     d xxDiv           s              5
     d xxFyr           s                   like(PAfyr)
     d xxCase          s                   like(PAcase)
     d svCase          s                   like(PAcase)
     d xxChk#          s                   like(PAchk#)
     d xxAmt           s                   like(PAamt)
     d xxRsvCode       s                   like(PArecd)
      * data structures/arrays
     d                 ds
     d aryRcvrCds                     2    dim(10)
     d  FNmep1
     d  FNcop1
     d  FNexp1
     d  FNlep1
     d                 ds
     d  cmrt                          2    dim(10)
     d  CMrt01                 1      2
     d  CMrt02                 3      4
     d  CMrt03                 5      6
     d  CMrt04                 7      8
     d  CMrt05                 9     10
     d  CMrt06                11     12
     d  CMrt07                13     14
     d  CMrt08                15     16
     d  CMrt09                17     18
     d  CMrt10                19     20
     d                 ds
     d  cmar                          9  2 dim(10)
     d  CMar01                 1      9  2
     d  CMar02                10     18  2
     d  CMar03                19     27  2
     d  CMar04                28     36  2
     d  CMar05                37     45  2
     d  CMar06                46     54  2
     d  CMar07                55     63  2
     d  CMar08                64     72  2
     d  CMar09                73     81  2
     d  CMar10                82     90  2
     d                 ds
     d  cmrr                          9  2 dim(10)
     d  CMrr01                 1      9  2
     d  CMrr02                10     18  2
     d  CMrr03                19     27  2
     d  CMrr04                28     36  2
     d  CMrr05                37     45  2
     d  CMrr06                46     54  2
     d  CMrr07                55     63  2
     d  CMrr08                64     72  2
     d  CMrr09                73     81  2
     d  CMrr10                82     90  2
     d                 ds
     d  rhrt                          2    dim(10)
     d  RHrt01                 1      2
     d  RHrt02                 3      4
     d  RHrt03                 5      6
     d  RHrt04                 7      8
     d  RHrt05                 9     10
     d  RHrt06                11     12
     d  RHrt07                13     14
     d  RHrt08                15     16
     d  RHrt09                17     18
     d  RHrt10                19     20
     d                 ds
     d  rhsu                          9  2 dim(10)
     d  RHsu01                 1      9  2
     d  RHsu02                10     18  2
     d  RHsu03                19     27  2
     d  RHsu04                28     36  2
     d  RHsu05                37     45  2
     d  RHsu06                46     54  2
     d  RHsu07                55     63  2
     d  RHsu08                64     72  2
     d  RHsu09                73     81  2
     d  RHsu10                82     90  2

     d psds          esds                  extname(SZQ1P)

      /free
       ChkStack('REVPOSTNEC':qqFound);
       If qqFound <> '1';
          *inlr = *on;
          Return;
       Endif;
       xxCo#  = %int(@@co#);
       xxTrDt = %int(@@trdt);
       If @@fnd <> *blank;
          xxFnd  = %int(@@fnd);
       Endif;
       If @@emp# <> *blank;
          xxEmp# = %int(@@emp#);
       Endif;
       xxDiv  = @@div;
       If @@fyr <> *blank;
          xxFyr  = %int(@@fyr);
       Endif;
       If @@fyr <> *blank;
          xxFyr  = %int(@@fyr);
       Endif;
       If @@case <> *blank;
          xxCase = %int(@@fyr);
       Endif;
       If @@chk# <> *blank;
          xxChk# = %int(@@chk#);
       Endif;
       Select;
       When xxCase <> 0;
          Setll (xxCo#:xxFnd:xxCase) ppa00;
          Reade (xxCo#:xxFnd:xxCase) ppa00;
       When xxFnd <> 0;
          Setll (xxCo#:xxFnd) ppa00;
          Reade (xxCo#:xxFnd) ppa00;
       Other;
          Setll (xxCo#) ppa00;
          Reade (xxCo#) ppa00;
       Endsl;
       Dow not(%eof);
       // Get out if OPNQRYF failed
          If xxTrdt <> PAtrdt;
             *Inlr = *on;
             Return;
          Endif;
       // Run thru filters
          If xxFnd <> svFnd;
             svFnd = xxFnd;
            // Populates aryRcvrCds
             Chain (PAco#:PAfnd) pfn00;
             If not %found;
                Exsr ReadFile;
                Iter;
             Endif;
          Endif;
          If xxCase <> svCase;
             svCase = xxCase;
             Chain (PAco#:PAfnd:PAfyr:PAcase) pcm01;
             If not %found;
                Exsr ReadFile;
                Iter;
             Endif;
          Endif;
          If xxChk# <> 0 and xxChk# <> PAchk#;
             Exsr ReadFile;
             Iter;
          Endif;
          If xxEmp# <> 0 and xxEmp# <> CMemp#;
             Exsr ReadFile;
             Iter;
          Endif;
          If xxDiv <> *blank and CMdiv <> %int(xxDiv);
             Exsr ReadFile;
             Iter;
          Endif;
          If xxFyr <> 0 and xxFyr <> CMfyr;
             Exsr ReadFile;
             Iter;
          Endif;
          Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel00;
          If not %found;
             Exsr ReadFile;
             Iter;
          Endif;
          Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:PAchk#) pdq00;
          If not %found;
             Exsr ReadFile;
             Iter;
          Endif;
       // Process record
          If not(ProcessPayment());
             Exsr ReadFile;
             Iter;
          Endif;
       Exsr ReadFile;
       Enddo;
       *Inlr = *on;
       Return;
       //===================================
       Begsr ReadFile;
       //===================================
       Select;
       When xxCase <> 0;
          Reade (xxCo#:xxFnd:xxCase) ppa00;
       When xxFnd <> 0;
          Reade (xxCo#:xxFnd) ppa00;
       Other;
          Reade (xxCo#) ppa00;
       Endsl;
       Endsr;
      /end-free
       //===================================================
     p GetRsvCode      b
       //===================================================
     d GetRsvCode      pi             2
     d  x                             2  0
      /free
       Select;
       When x = 1;
          Return 'ME';
       When x = 2;
          Return 'CO';
       When x = 3;
          Return 'EX';
       When x = 4;
          Return 'LE';
       Endsl;
      /end-free
     p GetRsvCode      e
       //===================================================
     p ProcessPayment  b
       //===================================================
     d ProcessPayment  pi              n
     d error           s               n
     d x               s              2  0
     d xxAmt           s              9  2
     d xxRsvCode       s              2
      /free
       Exsr GetRsvCd;
       // Return if not a net deductible recovery code
       If not(%found);
          Return *off;
       Endif;
       xxRsvCode = GetRsvCode(x);
       xxAmt     = PAamt;
       error = RevWDDDP(xxAmt:'1');
       If error = *on;
          WriteError('1');
          Return *off;
       Endif;
       error = RevWMCMP(xxAmt:'1');
       If error = *on;
          WriteError('2');
          Return *off;
       Endif;
       error = RevWADQP(xxAmt:'1');
       If error = *on;
          WriteError('3');
          Return *off;
       Endif;
       error = RevWDELP(xxAmt:'1');
       If error = *on;
          WriteError('4');
          Return *off;
       Endif;
       Write  ppa0p;
       If @@UpDate = 'Y';
          RevWDDDP(xxAmt:'2');
          RevWMCMP(xxAmt:'2');
          RevWADQP(xxAmt:'2');
          RevWDELP(xxAmt:'2');
          Delete ppa00;
       Endif;
       Return *on;
      /end-free

     c     getRsvCd      begsr
     c                   eval      x = 1
     c     PAbr2         lookup    aryRcvrCds(x)                          60
     c                   endsr

     p ProcessPayment  e
       //===================================================
     p RevWDDDP        b
       //===================================================
     d RevWDDDP        pi              n
     d  inAmt                              like(PAamt)
     d  inMode                        1    const
     d workAmt         s                   like(PAamt)
      /free
       workAmt = inAmt;
       Setgt  (PAco#:PAfnd:PAfyr:PAcase:PAfyr:PAtrdt) pdd04;
       Readpe (PAco#:PAfnd:PAfyr:PAcase:PAfyr) pdd04;
       Dow not(%eof);
          If xxRsvCode = DDrecd;
             ArcWDDDP();
             If workAmt > 0;
                If DDamtr >= workAmt;
                   DDamtr -= workAmt;
                   DDamtp += workAmt;
                   workAmt = 0;
                Else;
                   DDamtp  += DDamtr;
                   workAmt -= DDamtr;
                   DDamtr  =  0;
                Endif;
             Else;
                If DDamtb >= DDamtp + (workAmt * -1);
                   DDamtp += (workAmt * -1);
                   DDamtr -= (workAmt * -1);
                   workAmt = 0;
                ELse;
                   workAmt += (DDamtb - DDamtp);
                   DDamtp  += (workAmt * -1);
                   DDamtr  -= (workAmt * -1);
                Endif;
             Endif;
             If inMode = '2';
                DDpgm = Q1pgm;
                Update pdd04;
             Endif;
             If workAmt = 0;
                Leave;
             Endif;
          Endif;
       Readpe (PAco#:PAfnd:PAfyr:PAcase:PAfyr) pdd04;
       Enddo;
       If workAmt <> 0;
          Return *on;
       Else;
          Return *off;
       Endif;
      /end-free
     p RevWDDDP        e
       //===================================================
     p RevWMCMP        b
       //===================================================
     d RevWMCMP        pi              n
     d  inAmt                              like(PAamt)
     d  inMode                        1    const
     d x               s              2  0
      /free
       Chain (PAco#:PAfnd:PAfyr:PAcase) pcm01;
       If not(%found);
          Return *on;
       Endif;
       Exsr GetRsvCd;
       If not(%found);
          Return *on;
       Endif;
       ArcWMCMP();
       CMrr(x) += inAmt;
       CMar(x) -= inAmt;
       If CMar(x) < 0;
          Return *on;
       Endif;
       If CMstat = 'C' and inMode = '2';
          CMrr(x) = 0;
          WriteWMRHP();
       Endif;
       If inMode = '2';
          CMpgm = Q1pgm;
          Update pcm01;
       Else;
          Chain(n) (PAco#:PAfnd:PAfyr:PAcase) pcm01;
       Endif;
       Return *off;
      /end-free
      ****************************************
     c     getRsvCd      begsr
     c                   eval      x = 1
     c     xxRsvCode     lookup    cmrt(x)                                60
     c                   endsr
      ****************************************
     p RevWMCMP        e
       //===================================================
     p RevWADQP        b
       //===================================================
     d RevWADQP        pi              n
     d  inAmt                              like(PAamt)
     d  inMode                        1    const
      /free
       Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:PAchk#) pdq00;
       If not(%found);
          Return *on;
       Endif;
       ArcWADQP();
       DQamtr += inAmt;
       If DQamtr < 0;
          Return *on;
       Endif;
       If inMode = '2';
          DQpgm  = Q1pgm;
          Update pdq00;
       Else;
          Chain(n) (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:PAchk#) pdq00;
       Endif;
       Return *off;
      /end-free
     p RevWADQP        e
       //===================================================
     p RevWDELP        b
       //===================================================
     d RevWDELP        pi              n
     d  inAmt                              like(PAamt)
     d  inMode                        1    const
      /free
       Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel00;
       If not(%found);
          Return *on;
       Endif;
       ArcWDELP();
       If CMstat <> 'C';
          ELouts += PAamt;
       Endif;
       ELtclp -= PAamt;
       If inMode = '2';
          Update pel00;
       ELse;
          Chain(n) (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel00;
       Endif;
       Return *off;
      /end-free
     p RevWDELP        e
       //===================================================
     p WriteError      b
       //===================================================
     d WriteError      pi
     d  inMode                        1    const
      /free
       Select;
       When inMode = '1';
          Error = 'Issue with WDDDP';
       When inMode = '2';
          Error = 'Issue with WMCMP';
       When inMode = '3';
          Error = 'Issue with WADQP';
       When inMode = '4';
          Error = 'Issue with WDELP';
       Endsl;
       Write perror;
      /end-free
     p WriteError      e
       //==================================================
     p ArcWADQP        b
       //==================================================
      /free
       Write pdq0p;
      /end-free
     p ArcWADQP        e
       //==================================================
     p ArcWDDDP        b
       //==================================================
      /free
       Chain (DDco#:DDfnd:DDfyr:DDcase:DDbfyr:DDpodt:DDrecd) pdd00;
       If not(%found);
          Write pdd0p;
       Endif;
      /end-free
     p ArcWDDDP        e
       //==================================================
     p ArcWDELP        b
       //==================================================
      /free
       Chain (CMco#:CMfnd:CMfyr:CMemp#:CMdiv) pel01;
       If not(%found);
          Write pel0p;
       Endif;
      /end-free
     p ArcWDELP        e
       //==================================================
     p ArcWMCMP        b
       //==================================================
      /free
       Chain (DDco#:DDfnd:DDemp#:DDdiv:DDfyr:DDcase) pcm00;
       If not(%found);
          Write pcm0p;
       Endif;
      /end-free
     p ArcWMCMP        e
       //==================================================
     p WriteWMRHP      b
       //==================================================
     d x               s              2  0
      /free
       Clear prh0p;
       Exsr getRsvCd;
       RHco# = PAco#;
       RHfnd = PAfnd;
       RHfyr = PAfyr;
       rhrt(x) = cmrt(x);
       rhsu(x) = PAamt;
       RHexpl = 'Reverse Recovery';
       RHstat = *blank;
       RHclst = *blank;
       RHuser = Q1user;
       RHpgm  = Q1pgm;
       RHdt   = Dateto7(SystemDate());
       RHidt  = RHdt;
       RHtime = Timeto6(SystemTime());
       RHitim = RHtime;
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
       Write prh0p;
      /end-free
      *******************************************
     c     getRsvCd      begsr
     c                   eval      x = 1
     c     xxRsvCode     lookup    cmrt(x)                                60
     c                   endsr
      *******************************************
     p WriteWMRHP      e
       //==================================================
