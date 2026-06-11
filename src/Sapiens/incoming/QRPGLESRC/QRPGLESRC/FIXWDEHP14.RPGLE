     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*========================================================================
     H* Purpose - Correct written premium numbers in Premium
     H*           History records that were corrupted by Unbilled Premium records
     H*           in WXELP before the program fix for 86797 was installed
     H*           to client. These record's EHFAF and EHFAF2 values will
     H*           be 'E' and *blank respectively. At this point, W8010R
     H*           had this correction but it wasn't put into W8011R and
     H*           W7720R. Audit billing was not impacted by this issue, just
     H*           revised billing. This issue was introduced in V13R2.
     H*
     H* Parms -   If the entry parm is passed in as a 'Y', the program will
     H*           update the records. It always writes out the policies with
     H*           the condition explained below to output file FIXWDEHP14.
     H*
     H*           This program will correct written premium values in WDEHP
     H*           for records written recently (> 1131001) that have EHFAF
     H*           = 'E' and EHFAF2 = ' ' if a bill hasn't been afterwards.
     H*           Subsequent bills would true up the CEU. Running this
     H*           fix program may not be appropriate for every client because
     H*           it will adjust past CEU Reported numbers. It may be more
     H*           appropriate for them to re-bill the policy in order to
     H*           true up premium so that these changes can be viewed
     H*           on a transactional basis.
     H*
     H*           Regards: C:136
     H*========================================================================
    H*  REVISIONS:
    H*
     H*========================================================================
     fwdehp     uf   e             disk    rename(peh01:peh0p)
    f* employer detail history file
     fwdehl12   if   e           k disk    rename(peh01:peh12) prefix(l12)
    f* employer detail history file by co#/fnd/emp#/div/fyr/trdt
     fwdell     if   e           k disk    rename(pel01:pel0l)
    f* employer detail history file by co#/fnd/emp#/div/fyr
     ffixwdehp14uf a e             disk    rename(peh01:peh14)

     d clientID        s              3

     d psds          esds                  extname(szq1p)

     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
     d UpdateWtnPrm    pr
     d UpdateChgPrm    pr                  extpgm('FIXWDEHP')
     d   qqupd                        1    const
     d   qqco#                        3    const
     d   qqfnd                        3    const
     d   qqemp#                       9    const
     d   qqdiv                        5    const
     d   qqfyr                        3    const

     c     *entry        plist
     c                   parm                    chgMode           1

      /free
       // Clear changed record file
       Read peh14;
       Dow not(%eof);
       Delete peh14;
       Read peh14;
       Enddo;
       GetClientID(clientID);
       Read peh0p;
       Dow Not(%eof);
          If EHtrdt > 1130831 and EHfaf = 'E' and EHfaf2 = *blank;
             // Write "before change" record
             Write peh14;
             Setgt (EHco#:EHfnd:EHemp#:EHdiv:EHfyr:EHtrdt:EHtime) peh12;
             Reade (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) peh12;
             If %Eof;
                Chain (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) pel0l;
                If chgMode = 'Y';
                   UpdateWtnPrm();
                   UpdateChgPrm('Y':
                                %editc(EHco#:'X'):
                                %editc(EHfnd:'X'):
                                %editc(EHemp#:'X'):
                                %editc(EHdiv:'X'):
                                %editc(EHfyr:'X'));
                Endif;
             Else;
                If chgMode = 'Y';
                   EHfaf = 'R';
                   EHfaf2 = *blank;
                   EHpgm = %subst(EHpgm:1:9) + 'Z';
                   Update peh0p;
                Endif;
             Endif;
             // Write "after change" record ('Z' in last position of EHPGM)
             If chgMode = 'Y';
                Write peh14;
             Endif;
          Endif;
       Read peh0p;
       Enddo;
       *Inlr = *On;
       Return;
      /end-free

      *==============================================================
      * UpdateWtnPrm - calls FIXWDEHP to fix chg amounts in WDEHP for policy.
      *                EHFAF updated to 'R' since revised billing
      *                is the only process that can write EHFAF = 'E'
      *                and EHFAF2 = ' '
      *==============================================================
     p UpdateWtnPrm    b
     d UpdateWtnPrm    pi

     c                   move      'R'           ehfaf
     c                   move      elbscd        ehbscd
     c                   move      elpfdt        ehpfdt
     c                   move      elptdt        ehptdt
     c                   move      elandt        ehandt
     c                   move      elpror        ehpror
     c                   move      elmapr        ehwmpr
     c                   move      elstpr        ehwspr
     c                   move      elenmp        ehwnmp
     c                   move      elanpr        ehwapr
     c                   move      elemnp        ehwmnp
     c                   move      elprtd        ehprtd
     c                   move      elprtf        ehprtf
     c                   move      elwvfl        ehwvfl
     c                   move      elasp1        ehasp1
     c                   move      elcref        ehcref
     c                   move      elcrex        ehcrex
     c                   move      eledc1        ehwdc1
     c                   move      elasp2        ehasp2
     c                   move      eld1s2        ehd1s2
     c                   move      eld1d2        ehd1d2
     c                   move      eld1x2        ehd1x2
     c                   move      eld1e2        ehd1w2
     c                   move      eld2s2        ehd2s2
     c                   move      eld2d2        ehd2d2
     c                   move      eld2x2        ehd2x2
     c                   move      eld2e2        ehd2w2
     c                   move      eldvef        ehdvef
     c                   move      eldvex        ehdvex
     c                   move      eledc2        ehwdc2
     c                   move      elasp3        ehasp3
     c                   move      eldisc        ehdisc
     c                   move      eledc3        ehwdc3
     c                   move      elehcn        ehwhcn
     c                   move      elhcn         ehhcn
     c                   move      elexm1        ehexm1
     c                   move      elemd1        ehemd1
     c                   move      elexp1        ehwxp1
     c                   move      elexm2        ehexm2
     c                   move      elemd2        ehemd2
     c                   move      elexp2        ehwxp2
     c                   move      elexm3        ehexm3
     c                   move      elemd3        ehemd3
     c                   move      elexp3        ehwxp3
     c                   move      elilcd        ehilcd
     c                   move      elilef        ehilef
     c                   move      elilex        ehilex
     c                   move      elein1        ehwin1
     c                   move      elinl2        ehi2cd
     c                   move      eli2ef        ehi2ef
     c                   move      eli2ex        ehi2ex
     c                   move      elein2        ehwin2
     c                   move      eldcde        ehdcde
     c                   move      elddct        ehddct
     c                   move      elcxbl        ehcxbl
     c                   move      eldcef        ehdcef
     c                   move      eldcex        ehdcex
     c                   move      eledct        ehwdct
     c                   move      eldcd2        ehdcd2
     c                   move      eldd2p        ehdd2p
     c                   move      el2def        eh2def
     c                   move      el2dex        eh2dex
     c                   move      eledd2        ehwdd2
     c                   move      elaspm        ehaspm
     c                   move      elmcef        ehmcef
     c                   move      elmcex        ehmcex
     c                   move      eledcm        ehwdcm
     c                   move      elasps        ehasps
     c                   move      elscef        ehscef
     c                   move      elscex        ehscex
     c                   move      eledcs        ehwdcs
     c                   move      elrtcd        ehrtcd
     c                   move      elstat        ehstat
     c     clientID      ifeq      '025'
     c                   move      elstfc        ehstfc
     c                   else
     c                   move      eldev2        ehdev2
     c                   endif                                                  cltid = '025'
     c                   move      eledv2        ehwdv2
     c                   move      eld2ef        ehd2ef
     c                   move      eld2ex        ehd2ex
     c                   move      eldbcr        ehdbcr
     c                   move      elcmul        ehcmul
     c                   move      eledcr        ehwdcr
     c                   move      eledc         ehwdc
     c                   move      elesbj        ehwsbj
     c                   move      elempm        ehwmpm
     c                   move      elefrs        ehwfrs
     c                   move      eltxpr        ehtxpr
     c                   move      elavcc        ehavcc
     c                   move      elein2        ehwin2
     c                   move      eldbcr        ehdbcr
     c                   move      elcmul        ehcmul
     c                   move      elsdef        ehsdef
     c                   move      elsdex        ehsdex
     c                   move      eledcr        ehwdcr
     c                   move      ellfef        ehlfef
     c                   z-add     elewav        ehwwav
     c                   move      elnwrw        ehnwrw
     c                   z-add     eleter        ehwter
     c                   z-add     elterr        ehwtrr
     c                   z-add     elvlca        ehwvlc
     c                   z-add     eledtr        ehwdtr
     c                   z-add     eldtrr        ehwdrr
     c                   z-add     elhxmn        ehwhxm
     c                   z-add     elearc        ehwarc
     c                   z-add     elarcp        eharcp
     c                   z-add     elarca        eharca
     c                   z-add     elaref        eharef
     c                   z-add     elarex        eharex
     c                   z-add     elrpe1        ehwrp1
     c                   z-add     elrpe2        ehwrp2
     c                   z-add     elrpe3        ehwrp3
     c                   move      'Z'           ehpgm
     c                   update    peh0p
     p UpdateWtnPrm    e
