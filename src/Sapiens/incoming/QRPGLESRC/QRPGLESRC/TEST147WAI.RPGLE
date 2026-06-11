     hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
      /title Test147WAI - Test Pgm for Dividend Plan API
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp(*CALLER)
     h bnddir('WTBNDDIR')
      *  Created 06/13/13   C:136   Project: 147WAI
      *=========================================================================
      * Description - Is called with policy key info and Test Plan #. Writes out
      *               and updates files to comply with the necessary conditions
      *               required for the Test Plan # passed into the program.
      *               Conditions are stored in I:\CLT147\147WAI Condition Study.xlsx.
      *=========================================================================
     fwdell1    uf a e           k disk    rename(pel01 : pel0l1)
     fwmeml     uf a e           k disk    rename(pem01 : pem0l)
     fwtdvl11   uf a e           k disk    rename(pdv01 : pdv0l11)
     fwddvl     uf a e           k disk    rename(pdv01 : pdv0l)
     fqsysprt   o    f  135        printer oflind(*inof)
      *=========================================================================
      * Entry parms
      *=========================================================================
     d*entry
     d Test147WAI      pr
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inFyr                              like(ELfyr)
     d  inScript#                     3  0
     d Test147WAI      pi
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inFyr                              like(ELfyr)
     d  inScript#                     3  0
      *=================================================
      * Data Structures
      *=================================================
     d refWMEAP      e ds                  extname(WMEAP)
      *=================================================
      * Prototypes
      *=================================================
     d/copy sprDate
     d/copy wprDivPlan
     d Print           pr
     d UpdateWDDVP     pr
     d  inMinLvlPrm                        like(DVmnlv) const
     d  inMaxLvlPrm                        like(DVmxlv) const
     d UpdateWDELP     pr
     d  inDiv                              like(ELdiv)  const
     d  inPfdt                             like(ELpfdt) const
     d  inPtdt                             like(ELptdt) const
     d  inFAF                              like(ELfaf)  const
     d  inProRata                          like(ELpror) const
     d  inArSumFlg                         like(ELarsm) const
     d  inNormPrm                          like(ELenmp) const
     d  inMinPremFlg                       like(ELemnp) const
     d  inDivCode                          like(ELdvcd) const
     d  inDivPct                           like(ELdvpc) const
     d  inDeferredFlg                      like(ELdvdf) const
     d UpdateWMEMP     pr
     d  inDiv                              like(EMdiv)  const
     d  inEmStrp                           like(EMstrp) const
     d  inEfd1                             like(EMefd1) const
     d  inCad1                             like(EMcad1) const
     d UpdateWTDVP     pr
     d  inEfdt                             like(DVefdt) const
     d  inExdt                             like(DVexdt) const
     d  inPlanTypeFlg                      like(DVptyp) const
     d WriteFiles      pr
      *=================================================
      * Stand Alone Fields
      *=================================================
     d flgARSummry     s              1
     d flgCx           s              1
     d flgDeferred     s              1
     d flgFaf          s              1
     d flgInEfDtRng    s              1
     d flgInPrmRng     s              1
     d flgMinPrem      s              1
     d flgMode         s              1
     d flgPlanType     s              1
     d flgQte          s              1
     d inCad1          s                   like(EMcad1) inz(0)
     d inDivPct        s                   like(ELdvpc)
     d inEnv           s              1    inz('L')
     d intMaxPrmLvl    s                   like(DVmxlv)
     d intMinPrmLvl    s                   like(DVmnlv)
     d inPlanEffDt     s                   like(DVefdt)
     d inPlanExpDt     s                   like(DVexdt)
     d inProRata       s                   like(ELpror)
     d inPfdt          s                   like(ELpfdt)
     d inPtdt          s                   like(ELptdt)
     d inEmStRpt       s                   like(EMstrp) inz('Y')
     d intCtr          s              5  0 inz(1)
     d intDiv          s              5  0
     d intNormPrem     s                   like(ELenmp)
     d outResult       s               n
     d outDivBill      s              9  2
     d prtFlgMinPrm    s                   like(ELemnp)
     d prtNormPrem     s                   like(ELenmp)
     d strPlanID       s                   like(ELdvcd) inz('Test147WAI')
     d sysDate         s                   like(EMdt)
     d #ofDivs         s                   like(EMdiv) inz(3)
      *=================================================
      * Main
      *=================================================
      /free

       sysDate = DateTo7(SystemDate);
       WriteFiles();
       inPfdt = %int(%editc(inFyr : 'X') + '0101');
       inPtdt = %int(%editc(inFyr + 1 : 'X') + '0101');
       *inof = *on;

       Select;
       When inScript# = 1;
          flgMode       = '1';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 2;
          flgMode       = '2';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 3;
          flgMode       = '3';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 4;
          flgMode       = '4';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 5;
          flgMode       = '2';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'Y';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 6;
          flgMode       = '3';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'Y';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 7;
          flgMode       = '4';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'Y';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 8;
          flgMode       = '2';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'Y';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = inPfdt;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 9;
          flgMode       = '3';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'Y';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'N';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = inPfdt;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 10;
          flgMode       = '4';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'Y';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = inPfdt;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 11;
          flgMode       = '2';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'N';
          flgQte        = 'L';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 12;
          flgMode       = '3';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'N';
          flgQte        = 'L';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 13;
          flgMode       = '4';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'N';
          flgQte        = 'L';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 14;
          flgMode       = '2';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'N';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt + 1;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 15;
          flgMode       = '3';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'N';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt + 1;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 16;
          flgMode       = '4';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'N';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt + 1;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 17;
          flgMode       = '2';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'N';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs - 1;
          exsr SrUpdateFiles;

       When inScript# = 18;
          flgMode       = '3';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'N';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs - 1;
          exsr SrUpdateFiles;

       When inScript# = 19;
          flgMode       = '4';
          flgFaf        = 'E';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'N';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs - 1;
          exsr SrUpdateFiles;

       When inScript# = 20;
          flgMode       = '1';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs + 1;
          exsr SrUpdateFiles;

       When inScript# = 21;
          flgMode       = '2';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs + 1;
          exsr SrUpdateFiles;

       When inScript# = 22;
          flgMode       = '3';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs + 1;
          exsr SrUpdateFiles;

       When inScript# = 23;
          flgMode       = '3';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs + 1;
          exsr SrUpdateFiles;

       When inScript# = 24;
          flgMode       = '2';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'Y';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 25;
          flgMode       = '3';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'Y';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 26;
          flgMode       = '4';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'Y';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 27;
          flgMode       = '2';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'Y';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = inPfdt;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 28;
          flgMode       = '3';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'Y';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'N';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = inPfdt;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 29;
          flgMode       = '4';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'Y';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = inPfdt;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 30;
          flgMode       = '2';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'N';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 31;
          flgMode       = '3';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'N';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 32;
          flgMode       = '4';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'N';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 33;
          flgMode       = '2';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'N';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt + 1;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 34;
          flgMode       = '3';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'N';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt + 1;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 35;
          flgMode       = '4';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'N';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt + 1;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 36;
          flgMode       = '2';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'N';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs - 1;
          exsr SrUpdateFiles;

       When inScript# = 37;
          flgMode       = '3';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'N';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs - 1;
          exsr SrUpdateFiles;

       When inScript# = 38;
          flgMode       = '4';
          flgFaf        = 'A';
          flgARSummry   = 'N';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'N';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs - 1;
          exsr SrUpdateFiles;

       When inScript# = 39;
          flgMode       = '2';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 40;
          flgMode       = '3';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 41;
          flgMode       = '4';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;

       When inScript# = 42;
          flgMode       = '2';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;

       When inScript# = 43;
          flgMode       = '3';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;

       When inScript# = 44;
          flgMode       = '4';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;

       When inScript# = 45;
          flgMode       = '2';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 46;
          flgMode       = '3';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 47;
          flgMode       = '4';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 48;
          flgMode       = '2';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'N';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 49;
          flgMode       = '3';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'N';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 50;
          flgMode       = '4';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'N';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 51;
          flgMode       = '2';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'N';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'N';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt + 1;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 52;
          flgMode       = '3';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'N';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt + 1;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 53;
          flgMode       = '4';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'N';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt + 1;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 54;
          flgMode       = '2';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'N';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem * (#ofDivs - 1) + 1;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 55;
          flgMode       = '3';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'N';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem * (#ofDivs - 1) + 1;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 56;
          flgMode       = '4';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'Y';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'N';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem * (#ofDivs - 1) + 1;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 57;
          flgMode       = '2';
          flgFaf        = 'A';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'N';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 58;
          flgMode       = '3';
          flgFaf        = 'A';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'N';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       When inScript# = 59;
          flgMode       = '4';
          flgFaf        = 'E';
          flgARSummry   = 'Y';
          flgMinPrem    = 'N';
          flgCx         = 'N';
          flgDeferred   = 'N';
          flgQte        = 'L';
          flgPlanType   = 'F';
          flgInEfDtRng  = 'Y';
          flgInPrmRng   = 'Y';
          inPlanEffDt   = inPfdt;
          inPlanExpDt   = 0;
          inCad1        = 0;
          inDivPct      = 10;
          inProRata     = 1;
          intNormPrem   = 5000;
          intMinPrmLvl  = intNormPrem;
          intMaxPrmLvl  = intNormPrem * #ofDivs;
          exsr SrUpdateFiles;
       // Make div 1 non-summarized and cx'd
          Chain (ELco# : ELfnd : ELfyr : ELemp# : 1) pel0l1;
          ELarsm = ' ';
          Update pel0l1;
          Chain (ELco# : ELfnd : ELemp# : 1) pem0l;
          EMcad1 = inPfdt;
          Update pem0l;

       Endsl;

       Setll    (inCO# : inFnd : inFyr : inEmp#) pel0l1;
       Reade(N) (inCO# : inFnd : inFyr : inEmp#) pel0l1;
       Dow not(%eof);
          Chain(N) (ELco# : ELfnd : ELemp# : ELdiv) pem0l;
          If ELarsm = 'Y' and EMstrp <> 'Y' and flgMode = '3';
             Reade (ELco# : ELfnd : ELfyr : ELemp#) pel0l1;
             Iter;
          Endif;
          outResult = ValidDivplan (ELco# : ELfnd : ELemp# :
           ELdiv : ELfyr : strPlanID);
          Select;
          When flgMode = '1';
          When flgMode = '2';
             outDivBill = CalcDividend (ELco# : ELfnd : ELemp# :
              ELdiv : ELfyr);
          When flgMode = '3';
             outDivBill = UpdateDividend (ELco# : ELfnd : ELemp# :
              ELdiv : ELfyr);
          When flgMode = '4';
             outDivBill = PrintDividend (ELco# : ELfnd : ELemp# :
              ELdiv : ELfyr : inEnv);
          Endsl;
          Print();
       Reade(N) (inCO# : inFnd : inFyr : inEmp#) pel0l1;
       Enddo;

       *inlr = *on;
       //========================================================
       // SrUpdateFiles subroutine
       //========================================================
       begsr srUpdateFiles;
       UpdateWTDVP (inPlanEffDt : inPlanExpDt : flgPlanType);
       UpdateWDDVP (intMinPrmLvl : intMaxPrmLvl);
       Setll (inCO# : inFnd : inFyr : inEmp#) pel0l1;
       Reade (inCO# : inFnd : inFyr : inEmp#) pel0l1;
       Dow not(%eof);
          If ELdiv <> 0;
             inEmStRpt = 'N';
          Endif;
          UpDateWMEMP (ELdiv : inEmStRpt : inPfdt : inCad1);
          UpDateWDELP (ELdiv : inPfdt : inPtdt : flgFaf : inProRata :
           flgARSummry : intNormPrem : flgMinPrem : strPlanID : inDivPct :
           flgDeferred);
       Reade (inCO# : inFnd : inFyr : inEmp#) pel0l1;
       EndDo;
       endsr;
      /end-free
     oqsysprt   e            printHdr    1  1
     o                                           10 'Test #'
     o                                           14 'Mod'
     o                                           18 'FND'
     o                                           29 'EMP#'
     o                                           35 'DIV'
     o                                           39 'FYR'
     o                                           44 'FAF'
     o                                           48 'SUM'
     o                                           52 'MIN'
     o                                           56 'CX'
     o                                           60 'DFR'
     o                                           64 'L/Q'
     o                                           68 'TYP'
     o                                           72 'DtR'
     o                                           76 'PmR'
     o                                           94 'Normal Prem'
     o                                          101 'Div %'
     o                                          105 'Qua'
     o                                          117 'Div Prem'
     o          e            printTest   1  1
     o                                            7 'Test'
     o                       inScript#           10
     o                       flgMode             14
     o                       ELfnd         3     18
     o                       ELemp#        3     29
     o                       ELdiv         3     35
     o                       ELfyr               39
     o                       ELfaf               43
     o                       ELarsm              47
     o                       prtFlgMinPrm        51
     o                       flgCx               55
     o                       flgDeferred         59
     o                       flgQte              63
     o                       DVptyp              67
     o                       flgInEfDtRng        71
     o                       flginPrmRng         75
     o                       prtNormPrem   3     94
     o                       ELdvpc        3    101
     o                       outResult          104
     o                       outDivBill    3    117
      *=================================================
      * Print
      *=================================================
     p Print           b
     d Print           pr

      /free
       If *inof;
         Except PrintHdr;
         *inof = *off;
       Endif;
       If ELFAF <> 'A';
          prtNormPrem  = ELenmp;
          prtFlgMinPrm = ELemnp;
       ELse;
          prtNormPrem  = ELanmp;
          prtFlgMinPrm = ELamnp;
       Endif;
       Except PrintTest;
      /end-free
     p Print           e
      *=================================================
      * updateWDDVP -
      *=================================================
     p UpdateWDDVP     b
     d UpdateWDDVP     pi
     d  inMinLvlPrm                        like(DVmnlv) const
     d  inMaxLvlPrm                        like(DVmxlv) const
      /free
       Chain (inCo# : strPlanID) pdv0l;
       DVmnlv = inMinLvlPrm;
       DVmxlv = inMaxLvlPrm;
       Update pdv0l;
      /end-free
     p UpdateWDDVP     e
      *=================================================
      * updateWDELP -
      *=================================================
     p UpdateWDELP     b
     d UpdateWDELP     pi
     d  inDiv                              like(ELdiv)  const
     d  inPfdt                             like(ELpfdt) const
     d  inPtdt                             like(ELptdt) const
     d  inFAF                              like(ELfaf)  const
     d  inProRata                          like(ELpror) const
     d  inArSumFlg                         like(ELarsm) const
     d  inNormPrm                          like(ELenmp) const
     d  inMinPremFlg                       like(ELemnp) const
     d  inDivCode                          like(ELdvcd) const
     d  inDivPct                           like(ELdvpc) const
     d  inDeferredFlg                      like(ELdvdf) const
     d intCtr          s              5  0 inz(1)
     d intDiv          s              5  0
      /free
       Chain (inCo# : inFnd : inFyr : inEmp# : inDiv ) pel0l1;
       ELpfdt = inPfdt;
       ELptdt = inPtdt;
       ELfaf  = inFaf;
       ELpror = inProRata;
       ELarsm = inArSumFlg;
       Clear ELenmp;
       Clear ELemnp;
       Clear ELanmp;
       Clear ELamnp;
       If ELFAF <> 'A';
          ELenmp = inNormPrm + (1000 * (ELdiv - 1));
          ELemnp = inMinPremFlg;
       ELse;
          ELanmp = inNormPrm + (1000 * (ELdiv - 1));
          ELamnp = inMinPremFlg;
       Endif;
       ELdvcd = inDivCode;
       ELdvpc = inDivPct + (1 * (ELdiv - 1));
       ELdvdf = inDeferredFlg;
       ELuser = 'Test ' + %editc(inScript# : 'X');
       Update pel0l1;
      /end-free
     p UpdateWDELP     e
      *=================================================
      * UpdateWMEMP - Sets Cx Date on selected divisions
      *=================================================
     p UpdateWMEMP     b
     d UpdateWMEMP     pi
     d  inDiv                              like(EMdiv)  const
     d  inStRpt                            like(EMstrp) const
     d  inEfd1                             like(EMefd1) const
     d  inCad1                             like(EMcad1) const
      /free
       Chain (inCo# : inFnd : inEmp# : inDiv ) pem0l;
       EMstrp = inStRpt;
       EMefd1 = inEfd1;
       EMcad1 = inCad1;
       EMuser = 'Test ' + %editc(inScript# : 'X');
       Update pem0l;
      /end-free
     p UpdateWMEMP     e
      *=================================================
      * UpdateWTDVP -
      *=================================================
     p UpdateWTDVP     b
     d UpdateWTDVP     pi
     d  inEfdt                             like(DVefdt) const
     d  inExdt                             like(DVexdt) const
     d  inPlanTypeFlg                      like(DVptyp) const
      /free
       Chain (inCo# : strPlanID) pdv0l11;
       DVefdt = inEfdt;
       DVexdt = inExdt;
       DVptyp = inPlanTypeFlg;
       Update pdv0l11;
      /end-free
     p UpdateWTDVP     e
      *=================================================
      * WriteFiles - WMEMP, WDELP, WTDVP, WDDVP
      *=================================================
     p WriteFiles      b
     d WriteFiles      pi
     d intCtr          s              5  0 inz(1)
     d intDiv          s              5  0
      /free
       For intCtr to #ofDivs;
          intDiv = intCtr - 1;
          Chain(N) (inCo# : inFnd : inEmp# : intDiv ) pem0l;
          If not(%found);
             EMco#  = inCo#;
             EMfnd  = inFnd;
             EMemp# = inEmp#;
             EMdiv  = intDiv;
             If intDiv = 0;
                EMstrp = 'Y';
             Else;
                EMstrp = *blank;
             Endif;
             Write pem0l;
          Endif;
          Chain(N) (inCo# : inFnd : inFyr : inEmp# : intDiv) pel0l1;
          If not(%found);
             ELco#  = inCo#;
             ELfnd  = inFnd;
             ELemp# = inEmp#;
             ELdiv  = intDiv;
             ELfyr  = inFyr;
             Write pel0l1;
          Endif;
          Chain(N) (inCo# : strPlanID) pdv0l11;
          If not(%found);
             DVco#  = inCo#;
             DVplid = strPlanID;
             Write pdv0l11;
          Endif;
          Chain(N) (inCo# : strPlanID) pdv0l;
          If not(%found);
             DVco#  = inCo#;
             DVplid = strPlanID;
             Write pdv0l;
          Endif;
       EndFor;
      /end-free
     p WriteFiles      e
