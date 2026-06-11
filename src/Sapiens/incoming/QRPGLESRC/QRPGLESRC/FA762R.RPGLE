     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*****************************************************************
     H* This program will calculate medicare OPPS allowance for       *
     H* outpatient bills.                                             *
     H*****************************************************************
     H*  Created by:                                                  *
     H*  08/12/10  C:198 Project: 114MWS                              *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*  05/30/16 C:309 Project: FS497                                *
     H*                 Medicare OPPS status indicator Update         *
     H*                 Changed @@SI size to 2A  and included         *
     H*                 logic for status indicators J1-J2 & Q1-Q4     *
     H*                                                               *
     H*  04/06/16 C:309 Support 40307                                 *
     H*                 Lab fee calculation without L1 modifier       *
     H*                 calculate MRA using outlier, add outlier      *
     H*                 (xxOutl) only if APrel > 0                    *
     H*                                                               *
     H* 09/08/14 C:280 Project: WV013                                 *
     H*                Add logic to get the wage index form alternate *
     H*                tables for dates on or after 10/1/2010         *
     H*                change to use eowind and not opwind            *
     H*                                                               *
     H* 09/28/11 C:251 Project: 114MYS  Priss 54                      *
     H*                Rectify @@alow clearing issue                  *
     H*                                                               *
     H* 11/04/10 C:079 Project: WV009                                 *
     H*                Load SRDTI before converting the date          *
     H*                                                               *
     H* 10/01/10 C:198 Project: WV009                                 *
     H*                Changed to check for medicare release level    *
     H*                                                               *
     H*****************************************************************
     Ffdcap     if   e             disk
     Ffddul     if   e           k disk
     Ffaeul     if   e           k disk
     Fftrtl2    IF   E           K DISK
     Fftopl     if   e           k disk
     Fftapl     if   e           k disk
     Ffmrcl     if   e           k disk
     Fftuml     if   e           k disk
     F*** Beg Add *** 10/01/10 *************************************
     F*** Beg Chg *** 09/08/14 *************************************
     F*** faeolg    if   e           k disk
     Ffaeolg    uf   e           k disk
     F*** End Chg *** 09/08/14 *************************************
     F*** End Add *** 10/01/10 *************************************
     F*** Beg Add *** 09/08/14 *************************************
     Ffto2l1    if   e           k disk
     Fftoal1    if   e           k disk
     Fftobl1    if   e           k disk
     F*** End Add *** 09/08/14 *************************************
     F*
     F*
     D mod             s              2    dim(4)
     D xxst            s                   like(rtst)
     D xxtype          s                   like(rttype)
     D kkefdt          s                   like(rtefdt)
     D kkhcpc          s                   like(aphcpc)
     D*** RM *** xxcpt4          s                   like(prcpt4)
     D kpprov          s                   like(opprov)
     D kpefdt          s                   like(opdpt1)
     D @@st            s                   like(rtst)
     D @@type          s                   like(rttype)
     D xxtech          s                   like(camra)
     D xxprof          s                   like(camra)
     D xxmd26          s                   like(umod26)
     D xxmd27          s                   like(umod27)
     D @@bgdt          s                   like(euaddt)
     D xxcf            s              5  3
     D xxoutl          s              9  2
     D*
     D xxprov          s             10
     D*
     C*
     C     dtyymd        plist
     C                   parm                    srdti             8 0
     C                   parm      '*CYMD'       srffmt            8
     C                   parm      '*YYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
      *
     C     opky1         klist
     C                   kfld                    kpprov
     C                   kfld                    kpefdt
     C*
     C     opky2         klist
     C                   kfld                    kpprov
     C*
     C     apky1         klist
     C                   kfld                    kkhcpc
     C                   kfld                    kkefdt
     C*
     C     apky2         klist
     C                   kfld                    kkhcpc
     C*
     C     RTKY          KLIST
     C                   KFLD                    @@st
     C                   KFLD                    @@type
     C                   KFLD                    @@bgdt
     C*
     C     RTKY1         KLIST
     C                   KFLD                    @@st
     C                   KFLD                    @@type
     C*
     C     euky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C*
     C     duky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C*
     C     rcky1         klist
     C                   kfld                    caco#
     C                   kfld                    cazipa
     C                   kfld                    cacpt4
     C*** Beg Chg *** 10/01/10 *****************************************
     C******             kfld                    cafdat
     C                   kfld                    kkefdt
     C*** End Chg *** 10/01/10 *****************************************
     c*
     C     rcky2         klist
     C                   kfld                    caco#
     C                   kfld                    cazipa
     C                   kfld                    cacpt4
     C*
     C     umky1         klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C*** Beg Chg *** 10/01/10 *****************************************
     C*****              kfld                    cafdat
     C                   kfld                    kkefdt
     C*** End Chg *** 10/01/10 *****************************************
     C*
     C     umky2         klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C*
     C*** Beg Add *** 10/01/10 *************************************
     C     eoky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C*
     C*** End Add *** 10/01/10 *************************************
     C*** Main Processing
     C*
     C     *entry        plist
     C                   parm                    @@rrn             9 0
     C                   parm                    @@call            1
     C***** Beg Chg *** 05/30/16 **********************************************
     C*****              parm                    @@si              1
     C                   parm                    @@si              2
     C***** End Chg *** 05/30/16 **********************************************
     C                   parm                    @@eob             3
     C                   parm                    @@msg             7
     C                   parm                    @@modr            1
     C                   parm                    @@alow            9 2
     C                   parm                    @@rtcd            1
     C                   parm                    @@rsch            9 2
     C                   parm                    @@outl            9 2
     C*
     C     @@rrn         chain     pca01                              99
     C                   if        *in99 = *off
     C     euky          chain     peu01                              99
     C                   if        *in99 = *off
     C     duky          chain     pdu01                              60
     C                   if        *in99 = *off
     C                   movel     dupmod        mod(1)
     C                   movel     dumod2        mod(2)
     C                   movel     dumod3        mod(3)
     C                   movel     dumod4        mod(4)
     C                   eval      @@alow = 0
     C                   eval      @@outl = 0
     C                   eval      @@rsch = 0
     C                   eval      camra  = 0
     C                   exsr      OPPS_APC
     C                   select
     C                   when      @@call = 'N'
     C                   eval      @@alow = camra
     C                   eval      @@rsch = carsch
     C                   eval      @@outl = caoutl
     C*** Beg Add *** 10/01/10 *******************************************
     C                   when      @@call = 'F'
     C                   eval      @@alow = camra
     C                   eval      @@rsch = carsch
     C                   eval      @@outl = caoutl
     C*** End Add *** 10/01/10 *******************************************
     C                   endsl
     C                   endif                                                  duky
     C                   endif                                                  euky
     C                   endif                                                  fdcap
     C*
     C                   seton                                        lr
    C* ==================================================================
     C* Calculate  OPPS allowance for outpatient line items
    C* ==================================================================
     C     OPPS_APC      begsr
     C*
     C                   movel     eumcar        kpprov
     C*** Beg Add *** 10/01/10 **********************************************
     C                   z-add     0             xxcf
     C                   z-add     0             @@alw1
     C                   z-add     0             @@alow
     C                   z-add     0             @@rsch
     C                   z-add     0             @@outl
     C*
     C     eoky          chain     peo01
     C                   if        %found
     C                   Movel     eoast         @@st
     C                   endif
     C                   Movel     'O'           @@type
     C                   Movel     eudcdt        @@bgdt
     C*
     C     rtky          setgt     prt01                              99
     C     rtky1         readpe    prt01                                  99
     C*
     C                   dow       *in99 = *off
     C                   if        rtstat = 'I'
     C     rtky1         readpe    prt01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
     C*
     C                   if        rtmrel = '1'
     C                   z-add     rtefdt        kkefdt
     C                   z-add     rtefdt        srdti
     C                   else
     C*** End Add *** 10/01/10 **********************************************
     C                   z-add     cafdat        srdti
     C*** Beg Add *** 10/01/10 **********************************************
     C                   z-add     cafdat        kkefdt
     C                   endif
     C*
     C                   endif                                                  ftrtp if staement
     C*** End Add *** 10/01/10 **********************************************
     C*** Beg Add *** 11/04/10 **********************************************
     C                   z-add     cafdat        srdti
     C*** End Add *** 11/04/10 **********************************************
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         kpefdt
     C                   movel     cacpt4        kkhcpc
     C*
     C     opky1         setgt     pop01                              99
     C     opky2         readpe    pop01                                  99
     C*
     C                   if        *in99 = *off
     C*
     C*** Beg Add *** 09/08/14 ******************************************
     C*
      /free

           if cafdat < 1101001;
             eowind = opwind;
           else;
             exsr SrGetWageIndex;
           endif;

      /end-free
     C*
     C*** End Add *** 09/08/14 ******************************************
     C*
     C*Set the pointer for the FTRTP to get the rate values
     C*
     C                   Movel     'MC'          @@st
     C                   Movel     'O'           @@type
     C*** Beg Chg *** 10/01/10 ******************************************
     C******             Movel     cafdat        @@bgdt
     C                   Movel     eudcdt        @@bgdt
     C*** End Chg *** 10/01/10 ******************************************
     C*
     C     rtky          setgt     prt01                              99
     C     rtky1         readpe    prt01                                  99
     C                   if        *in99 = *off
     C*
     C                   eval      xxcf = RTAPC
     C                   if        (cafdat >= 1090101) and (ophoqu = *blank)
     C                   eval      xxcf = rtapc1
     C                   endif
     C*
     C                   if        kkhcpc = *blanks
     C                   eval      @@eob = '394'
     C                   eval      @@alow = 0
     C                   endif
     C*
     C                   movel     durate        kkhcpc
     C                   move      cafdat        kkefdt
     C*
     C     apky1         setgt     pap01                              99
     C     apky2         readpe    pap01                                  99
     C*
     C                   if        *in99 = '0'
     C*
     C****** Beg Chg *** 05/30/16 ***********************************************
     C******             eval      @@si = apsi
     C                   eval      @@si = apcsi
     C****** End Chg *** 05/30/16 ***********************************************
     C*
     C                   select
     C*
    C****** Beg Chg *** 05/30/16 ***********************************************
     C******             when      apsi = 'A' or apsi = 'B'
     C******                        or apsi = 'D' or apsi = 'D'
     C******                        or apsi = 'E' or apsi = 'F'
     C******                        or apsi = 'L' or apsi = 'M'
     C******                       or apsi = 'Y'
     C*
     C                   when         apcsi = 'A' or apcsi = 'B'
     C                             or apcsi = 'D' or apcsi = 'D'
     C                             or apcsi = 'E' or apcsi = 'F'
     C                             or apcsi = 'L' or apcsi = 'M'
     C                             or apcsi = 'Y'
    C****** End Chg *** 05/30/16 ***********************************************
     C                   select
     C                   when      @@call = 'N'
     C                   exsr      recu
     C                   when      @@call = 'F'
     C*** use logic from physician API FA763R
     C                   Call      'FA763R'
     C                   parm                    @@rrn
     C                   parm                    @@call
     C                   parm                    @@eob1            3
     C                   parm                    @@msg1            7
     C                   parm                    @@modr1           1
     C                   parm                    @@rnd1            1
     C                   parm                    @@alw1            9 2
     C                   parm                    @@alw2            9 2
     C                   parm                    @@alw3            9 2
     C                   parm                    @@rtcd1           1
     C                   eval      @@alow = @@alw1
     C*** Beg Add *** 09/28/11 **************************************************
     C                   eval      camra  = @@alow
     C*** End Add *** 09/28/11 **************************************************
     C                   endsl
     C                   eval      @@eob = '395'
     C*
    C****** Beg Chg *** 05/30/16 ***********************************************
     C******             when      apsi = 'H' or apsi = 'K'
     C*** Beg Chg *** 10/01/10 **************************************************
     C********                      or apsi = 'P' or apsi = 'Q1'
     C********                      or apsi = 'Q2' or apsi = 'Q3'
     C********                      or apsi = 'P' or apsi = 'Q'
     C*** End Chg *** 10/01/10 **************************************************
     C********                      or apsi = 'R' or apsi = 'S'
     C********                      or apsi = 'T' or apsi = 'U'
     C********                      or apsi = 'V'  or apsi = 'X'
     C*
     C                   when         apcsi = 'H'  or apcsi = 'K'
     C                             or apcsi = 'P'  or apcsi = 'Q'
     C                             or apcsi = 'R'  or apcsi = 'S'
     C                             or apcsi = 'T'  or apcsi = 'U'
     C                             or apcsi = 'V'  or apcsi = 'X'
     C                             or apcsi = 'J1' or apcsi = 'J2'
     C                             or apcsi = 'Q1' or apcsi = 'Q2'
     C                             or apcsi = 'Q3' or apcsi = 'Q4'
    C****** End Chg *** 05/30/16 ***********************************************
     C                   exsr      proc
     C                   eval      @@alow = camra
     C                   if         @@alow = 0
     C                   select
     C                   when      @@call = 'N'
     C                   exsr      recu
     C                   when      @@call = 'F'
     C*** use logic from physician API FA763R
     C                   Call      'FA763R'
     C                   parm                    @@rrn
     C                   parm                    @@call
     C                   parm                    @@eob1            3
     C                   parm                    @@msg1            7
     C                   parm                    @@modr1           1
     C                   parm                    @@rnd1            1
     C                   parm                    @@alw1            9 2
     C                   parm                    @@alw2            9 2
     C                   parm                    @@alw3            9 2
     C                   parm                    @@rtcd1           1
     C                   eval      @@alow =  @@alw1
     C*** Beg Add *** 09/28/11 **************************************************
     C                   eval      camra  = @@alow
     C*** End Add *** 09/28/11 **************************************************
     C                   endsl
     C                   endif                                                  apalow = 0
     C                   eval      @@eob = '395'
     C                   select
    C****** Beg Chg *** 05/30/16 ***********************************************
     C*****              when      apsi  = 'T'
     C                   when      apcsi = 'T'
    C****** End Chg *** 05/30/16 ***********************************************
     c     '51'          lookup    mod                                    99
     C                   if        *in99 = *on
     C                   eval      camra  = camra * .5
     C                   eval      @@eob = '390'
     C                   endif
    C****** Beg Chg *** 05/30/16 ***********************************************
     C*****              when      apsi  = 'S'
     C                   when      apcsi = 'S'
    C****** End Chg *** 05/30/16 ***********************************************
     c     '51'          lookup    mod                                    99
     C                   if         *in99 = *on
     C                   endif
     C                   eval      @@eob = '395'
     C                   endsl
     C*
    C****** Beg Chg *** 05/30/16 ***********************************************
     C*****              when      apsi  = 'N'
     C                   when      apcsi = 'N'
    C****** End Chg *** 05/30/16 ***********************************************
     C                   eval       @@alow = 0
     C                   eval      @@eob = '393'
     C*
     C                   endsl
     C*
     C                   endif                                                  *in99 = *off
     C*
     C                   endif                                                  *in99 = *off
     C*
     C                   endif                                                  *in99 = *off
     C*
     C                   endsr
     C* ====================================================================================
     C     proc          begsr
     C*
     C                   eval      xxoutl = cachrg * opoccr
     C*
     C*** Beg Chg 09/08/14 ********************************************
     C*
     C****               if        opwind <> 0
     C                   if        eowind <> 0
     C*
     C*** End Chg 09/08/14 ********************************************
     C*
     C                   if        xxoutl > (3.5*apalow)
     C                   if        xxoutl > apalow
     C                   eval      xxoutl = .5 * (xxoutl - apalow)
     C                   else
     C                   eval      xxoutl = .5 * (apalow - xxoutl)
     C                   endif
     C                   else
     C                   eval      xxoutl = *zeros
     C                   endif
     C*** write to FDCAP/CAOUTL
     C                   eval      caoutl = xxoutl
     C                   If        optype = '16'
     C                             and %subst(opacbs : 1 : 3) = ' '
     C                             OR optype = '17'
     C                             and %subst(opacbs : 1 : 3) = ' '
     C*
     C                   select
     C*
     C                   when      @@call = 'F'
     C*
     C*
     C*** Beg Chg 09/08/14 ********************************************
     C*
     C*****              eval      camra = (((((opwind*.60*xxcf) +
     C*** Beg Chg *** 04/06/16 *******************************************
     C                   if        APrel > 0
     C                   eval      camra = (((((eowind*.60*xxcf) +
     C*
     C*** End Chg 09/08/14 ********************************************
     C*
     C                                     (.40*xxcf))*aprel)*1.071) +
     C                                      + xxoutl)
     C                   else
     C                   eval      camra = 0
     C                   endif
     C*
     C*****              eval      camra = (((((eowind*.60*xxcf) +
     C*****                                (.40*xxcf))*aprel)*1.071) +
     C*****                                 + xxoutl)
     C*** End Chg *** 04/06/16 *******************************************
     C                   when      @@call = 'N'
     C*
     C*** Beg Chg 09/08/14 ********************************************
     C*
     C****               eval      camra = (((((opwind*.60*xxcf) +
     C*** Beg Chg *** 04/06/16 *******************************************
     C                   if        APrel > 0
     C                   eval      camra = (((((eowind*.60*xxcf) +
     C*
     C*** End Chg 09/08/14 ********************************************
     C*
     C                                     (.40*xxcf))*aprel)*1.071) +
     C                                      + xxoutl)
     C                   else
     C                   eval      camra = 0
     C                   endif
     C*
     C*****              eval      camra = (((((eowind*.60*xxcf) +
     C*****                                (.40*xxcf))*aprel)*1.071) +
     C*****                                 + xxoutl)
     C*** End Chg *** 04/06/16 *******************************************
     C*
     C                   endsl
     C*** write to FDCAP/CARSCH
     C*
     C*** Beg Chg 09/08/14 ********************************************
     C*
     C****               eval      carsch = (((opwind*.60*xxcf) +
     C                   eval      carsch = (((eowind*.60*xxcf) +
     C*
     C*** End Chg 09/08/14 ********************************************
     C*
     C                                        (.40*xxcf))*aprel)*1.071
     C                   else
     C*
     C                   select
     C*
     C                   when      @@call = 'F'
     C*
     C*** Beg Chg 09/08/14 ********************************************
     C*
     C****               eval      camra = ((((opwind*.60*xxcf) +
     C*** Beg Chg *** 04/06/16 *******************************************
     C                   if        APrel > 0
     C                   eval      camra = ((((eowind*.60*xxcf) +
     C*
     C*** End Chg 09/08/14 ********************************************
     C*
     C                                     (.40*xxcf))*aprel)        +
     C                                      + xxoutl)
     C                   else
     C                   eval      camra = 0
     C                   endif
     C*
     C*****              eval      camra = ((((eowind*.60*xxcf) +
     C*****                                (.40*xxcf))*aprel)        +
     C*****                                 + xxoutl)
     C*** Beg Chg *** 04/06/16 *******************************************
     C*
     C                   when      @@call = 'N'
     C*** Beg Chg 09/08/14 ********************************************
     C*
     C****               eval      camra = ((((opwind*.60*xxcf) +
     C*** Beg Chg *** 04/06/16 *******************************************
     C                   if        APrel > 0
     C                   eval      camra = ((((eowind*.60*xxcf) +
     C*
     C*** End Chg 09/08/14 ********************************************
     C*
     C                                     (.40*xxcf))*aprel)        +
     C                                      + xxoutl)
     C                   else
     C                   eval      camra = 0
     C                   endif
     C*
     C*****              eval      camra = ((((eowind*.60*xxcf) +
     C*****                                (.40*xxcf))*aprel)        +
     C*****                                 + xxoutl)
     C*** End Chg *** 04/06/16 *******************************************
     C*
     C                   endsl
     C*
     C                   endif                                                  optype = '16'
     C*
     C                   else                                                   opwind <> 0
     C*
     C                   eval      camra = apalow
     C*
     C                   endif                                                  opwind <> 0
     C*
     C     endtag        tag
     C*
     C     proce         endsr
    C* ==================================================================
     C     recu          begsr
    C*
    C*  Default to use service date for effective date for rules
     C*** Beg Del *** 10/01/10 **********************************************
     C*****              eval      kkefdt = cafdat
     C*** End Del *** 10/01/10 **********************************************
    C*
    C* Now that the Provider is established, get its Geographic Zip Area
    C* 1st - establish the first 3-bytes of the zip code
    C*
    C*
     C     rcky1         setgt     prc01
     C     rcky2         readpe    prc01                                  99
    C*
     C                   dow       *in99 = *off
     C                   if        rcstat = 'I'  and *in99 = *off
     C     rcky2         readpe    prc01                                  99
     C                   elseif    rcstat = 'A'
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
    C*
     C                   eval      camra  = rcmra
     C                   eval      xxprof = rcprof
     C                   eval      xxtech = rc92n1
     c*
     C                   endif                                                  end *in99
     c*
     C     umky1         setgt     pum01
     C     umky2         readpe    pum01                                  99
    C*
     C**** dow logic
     C                   dow       *in99 = *off
     C                   if        umstat = 'I'  and *in99 = *off
     C     umky2         readpe    pum01                                  99
     C                   elseif    umstat = 'A'
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
     C                             and cacpt4 <= umtocp
    C*
     C                   move      umod26        xxmd26
     C                   move      umod27        xxmd27
    C*
     C                   if        xxprof = 0
     C                   eval      xxprof = rcmra * xxmd26
     C                   endif
    C*
     C                   if        xxtech = 0
     C                   eval      xxtech = rcmra * xxmd27
     C                   endif
    C*
     C                   endif
    C*
     C     recue         endsr
    C*
     C*** Beg Add *** 09/08/14 ******************************************
    C*
    C*=========================================================================
    C*    GetWageIndex - Subroutine - Get Wage Indes
    C*=========================================================================
    C*

      /free

           begsr SrGetWageIndex;

           select;

           When opsppy = 'Y' or opsppy = '2';
             xxprov = opprov;

             setgt (xxprov:kkefdt) fto2l1;
             readpe (xxprov) fto2l1;

              if not %eof(fto2l1);
                eowind = o2crwi;
              endif;

           When opsppy <> 'Y' and opsppy <> '2';

             setgt (opacbs:kkefdt) ftoal1;
             readpe (opacbs) ftoal1;

              if not %eof(ftoal1);
                eowind = oacrwi;
              else;

              setgt (opacbs:kkefdt) ftobl1;
              readpe (opacbs) ftobl1;

                if not %eof(ftobl1);
                  eowind = obcrwi;
                endif;

              endif;

           endsl;

           endsr;

      /end-free
    C*
     C*** End Add *** 09/08/14 ******************************************
    C*
