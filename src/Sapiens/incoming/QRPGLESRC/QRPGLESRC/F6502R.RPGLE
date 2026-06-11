     hcopyright('(c) 2016 stoneriver, inc. all rights reserved. -
     h stoneriver is a trademark of stoneriver, inc.')
     h option(*nodebugio:*srcstmt)
     h dftactgrp(*no) actgrp('qile')
     h/define convert

      *   C6502r - upload performrx bill transfer program 2            *
      *   **************************************************************
      *                                                                *
      *   Called from: f6501r                                          *
      *   Calls to   : none                                            *
      *                                                                *
      *   **************************************************************
      *   Created on 04/12/2016 by c:212 for project 114mko            *
      *   **************************************************************
      *   Revisions:                                                   *
      *                                                                *
      *   02/06/17 C:212 Project: 114MEL                               *
      *                  Laod PerformRx Metric Quantity differently    *
      *                                                                *
      *   09/09/16 C:079 Support: 114/43012                            *
      *                  Change how reversals are processed            *
      *                  Change how charges are mapped                 *
      *                  Use reserve code ME not M                     *
      *                                                                *
      ******************************************************************
      *   Indicators used:                                             *
      *                                                                *
      ******************************************************************
     ffak1p     uf   e             disk    rename(pk101:pk10p)
     ffak1l1    uf   e           k disk
     f                                     rename(pk101:pk101)
     ffak1l4    uf   e           k disk
     f                                     rename(pk101:pk104)
     ffak9l2    uf   e           k disk

      ** header files
     ffaeol4    uf a e           k disk
     f                                     infds(dbfds)
     ffaeal1    if a e           k disk
     ffaeal37   if   e           k disk
     f                                     rename(pea01:pea02)
     ffaesl2    if a e           k disk

      ** detail files
     ffdcap     o    e             disk
     ffddal2    if a e           k disk
     ffddpl     if a e           k disk
     ffddpl2    if   e           k disk    rename(pdp01:pdp02)
     ffaepl     if a e           k disk

      ** check request file
     fwarpln    if a e           k disk
     fwarple    if   e           k disk    rename(prp01:prp03)
     fwdpala    if   e           k disk

      ** miscellaneous files
     ffdcalf    if   e           k disk
     f                                     rename(pca01:pca02)
     ffmc1l     uf   e           k disk
     f                                     rename(pc101:pm101)
     f                                     prefix(m1:02)
     ffmcml1    if   e           k disk
     ffmf1l     if   e           k disk
     ffmgzl     if   e           k disk
     ffmpdl     if   e           k disk
     fststl     if   e           k disk
     fwarpl     if   e           k disk
     f                                     rename(prp01:prp02)
     fwmcml1    if   e           k disk
     f                                     rename(pcm01:pcm02)
     f                                     prefix(cw:02)
     fwmcil     if   e           k disk
     fwtpsl     if   e           k disk
     ffddep     o    e             disk
     ffddel     if   e           k disk
     f                                     rename(pde01:pde02)
     fftprxl1   if   e           k disk
     fwtabl     if   e           k disk
     f                                     ignore(pat01)
     f                                     ignore(psp01)
     f                                     ignore(pnc01)
     f                                     ignore(pna01)
     f                                     ignore(ppa01)
     f                                     ignore(pca01)
     f                                     ignore(prc01)
     f                                     ignore(pb201)
     f                                     ignore(pb701)
     f                                     ignore(pbs01)
     f                                     ignore(ptt01)
     f                                     ignore(pcc01)
     f                                     ignore(pbc01)
     f                                     ignore(pcs01)
     f                                     ignore(pcn01)
     f                                     ignore(prs01)
     f                                     ignore(plr01)
     f                                     ignore(pvs01)
     f                                     ignore(ptr01)
     f                                     ignore(phy01)
     f                                     ignore(psy01)
     f                                     ignore(pba01)
     f                                     ignore(pid01)
     f                                     ignore(pis01)

      /copy scopyww01d
      //****************************************************************
      // program constants.
      //****************************************************************
     d comapi          c                   const('FA007R')
     d reas1           c                   const('PerformRx -
     d                                     bill upload')
     d lo              c                   const('abcdefghijklmnopqrst-
     d                                     uvwxyz')
     d alpha           c                   const('ABCDEFGHIJKLMNOPQRST-
     d                                     UVWXYZ')
     d fein            c                   const('270863878')
      //================================================================
      //  prototypes
      //================================================================
     d convrtamt       pr             9  2
     d inamt                          8

     d convrtamt4      pr            11  4
     d inamt                         10

     d***** Beg Add *** 02/06/17 ***************************************

     d convrtamt3      pr            11  3
     d inamt                         10
     d***** End Add *** 02/06/17 ***************************************

      //================================================================
      // user defined data structures
      //================================================================
     d                 ds
     d  claim#                 1     20
     d  xxwcf#                 1      3
     d  xxfyr                  4      6
     d  xxcase                 7     13

     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0

     d                 ds
     d  data                   1     75
     d  qxtrdt                 1      7  0
     d  qxfrdt                 8     14  0
     d  qxtodt                15     21  0
     d  qxtrcd                22     23
     d  qxrecd                24     25
     d  qxpv1                 26     27
     d  qxpv2                 28     29
     d  qxseq#                30     34  0
     d  qxeob#                35     43
     d  qxfein                44     52  0
     d  qxmod                 53     55  0
     d  qxdsq#                56     60  0

      //================================================================
      //  standalone
      //================================================================
     d yyamt           s              9  2 inz
     d xxadjseq        s              2  0 inz
     d xxadjcd         s              3    inz
     d inderr          s               n   inz(*off)
     d inddup          s               n   inz(*off)
     d xxtmpdt         s               d   inz
     d xxtmpdt1        s               d   inz
     d xxeob#          s                   like (K1eob#) inz
     d saveob#         s                   like (K1eob#) inz
     d xxdate          s              8  0 inz
     d***** Beg Add *** 02/06/17 *****************************************
     d xxamt3          s             11  3 inz
     d***** End Add *** 02/06/17 *****************************************

      //================================================================
      //  plist definitions
      //================================================================
     c     *entry        plist
     c                   parm                    @@mbr            10
     c                   parm                    @@bch             7
     c                   parm                    @@rrnk1           9 0

     c     dtcymd        plist
     c                   parm                    srdti             8 0
     c                   parm      *blanks       srffmt            8
     c                   parm      '*CYMD'       srtfmt            8
     c                   parm                    srdto            10
     c                   parm                    srerr             1

     c     dtcym2        plist
     c                   parm                    srdti
     c                   parm      '*YYMD'       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr

     c     compl         plist
     c                   parm                    ##co#             3 0
     c                   parm                    ##fnd             3 0
     c                   parm                    ##conm           40
     c                   parm                    ##fnam           40
     c                   parm                    ##sub#            5 0
     c                   parm                    ##fyr             3 0
      //================================================================
      //  klist definitions
      //================================================================

     c     m1ky          klist
     c                   kfld                    ##co#

     c     caky          klist
     c                   kfld                    K1co#
     c                   kfld                    K1sub#
     c                   kfld                    K1fnd
     c                   kfld                    K1fyr
     c                   kfld                    K1case
     c                   kfld                    kkdseq

     c     cpky          klist
     c                   kfld                    K1co#
     c                   kfld                    K1sub#
     c                   kfld                    K1fnd
     c                   kfld                    K1fyr
     c                   kfld                    K1case
     c                   kfld                    K1eob#

     c     capky1        klist
     c                   kfld                    K1co#
     c                   kfld                    K1sub#
     c                   kfld                    K1fnd
     c                   kfld                    K1fyr
     c                   kfld                    K1case
     c                   kfld                    kkeob#
     c                   kfld                    kkdseq

     c     eobky1        klist
     c                   kfld                    K1co#
     c                   kfld                    K1sub#
     c                   kfld                    K1fnd
     c                   kfld                    K1fyr
     c                   kfld                    K1case
     c                   kfld                    K1eob#

     c     ciky          klist
     c                   kfld                    K1co#
     c                   kfld                    kkwcf#
     c                   kfld                    kkptyp
     c                   kfld                    kksys

     c     caky2         klist
     c                   kfld                    K1co#
     c                   kfld                    K1sub#
     c                   kfld                    K1fnd
     c                   kfld                    K1fyr
     c                   kfld                    K1case

     c     caky3         klist
     c                   kfld                    K1co#
     c                   kfld                    K1sub#
     c                   kfld                    K1fnd
     c                   kfld                    K1fyr
     c                   kfld                    K1case
     c                   kfld                    K1eob#
     c                   kfld                    kklin#

     c     daky          klist
     c                   kfld                    eoco#
     c                   kfld                    eosub#
     c                   kfld                    eofnd
     c                   kfld                    eofyr
     c                   kfld                    eocase
     c                   kfld                    eoeob#
     c                   kfld                    cadseq
     c                   kfld                    eorcn

     c     cdky          klist
     c                   kfld                    kkss#

     c     cmky1         klist
     c                   kfld                    K1co#
     c                   kfld                    K1sub#
     c                   kfld                    K1fnd
     c                   kfld                    K1fyr
     c                   kfld                    K1case

     c     cmky2         klist
     c                   kfld                    ##co#
     c                   kfld                    kksub#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase

     c     cmky3         klist
     c                   kfld                    K1co#
     c                   kfld                    kkwcf#
     c                   kfld                    K1fyr
     c                   kfld                    K1case

     c     eaky1         klist
     c                   kfld                    ##co#
     c                   kfld                    kksinv

     c     eaky2         klist
     c                   kfld                    eoco#
     c                   kfld                    eosub#
     c                   kfld                    eofnd
     c                   kfld                    eofyr
     c                   kfld                    eocase
     c                   kfld                    eoeob#
     c                   kfld                    eorcn

     c     eobky         klist
     c                   kfld                    K1co#
     c                   kfld                    K1sub#
     c                   kfld                    K1fnd
     c                   kfld                    K1fyr
     c                   kfld                    K1case
     c                   kfld                    K1eob#

     c     eoky1         klist
     c                   kfld                    ##co#
     c                   kfld                    K1sinv

     c     eoky2         klist
     c                   kfld                    kkidoc

     c     esky          klist
     c                   kfld                    eoco#
     c                   kfld                    eosub#
     c                   kfld                    eofnd
     c                   kfld                    eofyr
     c                   kfld                    eocase
     c                   kfld                    eoeob#
     c                   kfld                    eorcn

     c     f1ky          klist
     c                   kfld                    K1co#
     c                   kfld                    K1sub#
     c                   kfld                    K1fnd

     c     gzky          klist
     c                   kfld                    K1co#
     c                   kfld                    kkzipa

     c     pdky1         klist
     c                   kfld                    kkfein
     c                   kfld                    kkmod

     c     pdky2         klist
     c                   kfld                    kkfein

     c     rpky1         klist
     c                   kfld                    K1co#
     c                   kfld                    kkwcf#
     c                   kfld                    K1fyr
     c                   kfld                    K1case
     c                   kfld                    kkeob#
     c                   kfld                    kkeobs

     c     rpky2         klist
     c                   kfld                    K1co#
     c                   kfld                    kkwcf#
     c                   kfld                    K1fyr
     c                   kfld                    K1case

     c     stky          klist
     c                   kfld                    kkst             10

     c     tbky          klist
     c                   kfld                    ##co#
     c                   kfld                    kkkey

     c     k1ky1         klist
     c                   kfld                    @@bch

     c     k1ky2         klist
     c                   kfld                    K1eob#

     c     deky1         klist
     c                   kfld                    caco#
     c                   kfld                    casub#
     c                   kfld                    cafnd
     c                   kfld                    cafyr
     c                   kfld                    cacase
     c                   kfld                    caeob#
     c                   kfld                    cadseq

     c     deky2         klist
     c                   kfld                    caco#
     c                   kfld                    casub#
     c                   kfld                    cafnd
     c                   kfld                    cafyr
     c                   kfld                    cacase
     c                   kfld                    caeob#
     c                   kfld                    cadseq
     c                   kfld                    kkseq#

      //================================================================
      //  *like defn statements
      //================================================================

     c     *like         define    cmco#         kkco#
     c     *like         define    cmsub#        kksub#
     c     *like         define    cmfnd         kkfnd
     c     *like         define    cmfyr         kkfyr
     c     *like         define    cmcase        kkcase
     c     *like         define    cmfnd         kkwcf#
     c     *like         define    eoeob#        kkeob#
     c     *like         define    cmss#         kkss#
     c     *like         define    cmacdt        kkacdt
     c     *like         define    pdfein        kkfein
     c     *like         define    pdmod         kkmod
     c     *like         define    cadseq        kkdseq
     c     *like         define    cadseq        xxdseq
     c     *like         define    rpseq#        xxseq#
     c     *like         define    pckey         kkkey
     c     *like         define    eoast         xxast
     c     *like         define    gzzipa        kkzipa
     c     *like         define    eorcn         kkrcn
     c     *like         define    rpeobs        kkeobs
     c     *like         define    calin#        kklin#
     c     *like         define    easinv        kksinv
     c     *like         define    eosinv        K1sinv
     c     *like         define    eosinv        xxsinv
     c     *like         define    eoidoc        kkidoc
     c     *like         define    ciptyp        kkptyp
     c     *like         define    cisys         kksys
     c     *like         define    eochrg        ohchrg
     c     *like         define    eomra         ohmra
     c     *like         define    eoothr        ohothr
     c     *like         define    eondis        ohndis
     c     *like         define    eopaid        ohpaid
     c     *like         define    eoinpe        ohinpe
     c     *like         define    eoinpn        ohinpn
     c     *like         define    eooute        ohoute
     c     *like         define    eooutn        ohoutn
     c     *like         define    eoasce        ohasce
     c     *like         define    eoascn        ohascn
     c     *like         define    eoclne        ohclne
     c     *like         define    eoclnn        ohclnn
     c     *like         define    eolabe        ohlabe
     c     *like         define    eolabn        ohlabn
     c     *like         define    eogrpe        ohgrpe
     c     *like         define    eogrpn        ohgrpn
     c     *like         define    eoinde        ohinde
     c     *like         define    eoindn        ohindn
     c     *like         define    cachrg        odchrg
     c     *like         define    camra         odmra
     c     *like         define    caothr        odothr
     c     *like         define    candis        odndis
     c     *like         define    capaid        odpaid
     c     *like         define    eopaid        xxamt
     c     *like         define    eofrdt        xxfrdt
     c     *like         define    eotodt        xxtodt
     c     *like         define    eanamt        hldamt
     c     *like         define    eanamt        kkpaid
     c     *like         define    cmast         svast
     c     *like         define    deseq#        kkseq#
     c     *like         define    deseq#        hdseq#
     c     *like         define    dprxdy        xxrxdy
     c     *like         define    dprxmq        xxamt4

      // execute main subroutine

      /free
       exsr mainsr;

       //================================================================
       // errck - error check subroutine
       //================================================================
       begsr errck;

         // initialize
         inderr = *off;
         inddup = *off;

           // check wcmc claim master to see if claim number exists

           exsr getclm;
           Chain cmky2 pcm01;
           *in99 = not %found;
           if not %found (fmcml1);
             xxercd += 1;
             inderr = *on;
           endif;

           // check for duplicate payments
           select;
            when K1clmtyp = 'A';
             xxamt = convrtamt(K1amtadj);
            when K1clmtyp = 'D';
             xxamt = convrtamt(K1amtrej);
            when K1clmtyp = 'P';
             xxamt = convrtamt(K1paid);
           endsl;

           if xxamt <> *zeros;

      /end-free
     c                   movel     K1fdat        xxdate            8 0
     c                   z-add     xxdate        srdti
     c                   call      'SRCVTDAT'    dtcym2
     c                   movel     srdto         xxfrdt

     c                   movel     K1fdat        xxdate
     c                   z-add     xxdate        srdti
     c                   call      'SRCVTDAT'    dtcym2
     c                   movel     srdto         xxtodt

      // check for duplicate request/payment

     c                   call      'WA052R'
     c                   parm      ##co#         $#co#             3 0
     c                   parm      kkwcf#        $#fnd             3 0
     c                   parm      kkfyr         $#fyr             3 0
     c                   parm      kkcase        $#case            7 0
     c                   parm      xxfrdt        $#from            7 0
     c                   parm      xxtodt        $#to              7 0
     c                   parm      xxamt         $#amt             9 2
     c*** Beg Chg *** 09/09/16 ********************************
     c****************   parm      'M '          $#recd            2
     c                   parm      'ME'          $#recd            2
     c*** End Chg *** 09/09/16 ********************************
     c                   parm      K1pfei        $#fein            9 0
     c                   parm      'M'           $#retp            1
     c                   parm      *blanks       $#rtcd            2

      /free
             if $#rtcd <> *blanks;
               if $#rtcd = 'DP' or $#rtcd = 'DR';
                 Setll (K1co#:K1sub#:K1fnd:K1fyr:K1case:K1ndc) pdp02;
                 Reade (K1co#:K1sub#:K1fnd:K1fyr:K1case:K1ndc) pdp02;
                 Dow not %eof (fddpl2);
                   monitor;
                    xxtmpdt = %date(K1fdat:*iso0);
                   on-error;
                    xxtmpdt = *loval;
                   endmon;

                   monitor;
                    xxtmpdt1= %date(dprxfd:*cymd);
                   on-error;
                    xxtmpdt1= *loval;
                   endmon;

                   if xxtmpdt1 = xxtmpdt;

                     Chain(n) (K1co#:K1fnd:K1fyr:K1case:dpeob#) prp03;
                     yyamt = convrtamt(K1paid);
                     if %found(warple) and rpamt = yyamt;
                       inderr = *on;
                       inddup = *on;
                       xxdup += 1;
                       leave;
                     else;
                       Chain(n) (K1co#:K1fnd:K1fyr:K1case:dpeob#) ppa01;
                       yyamt = convrtamt(K1paid);
                       if %found(wdpala) and paamt = yyamt;
                         inderr = *on;
                         inddup = *on;
                         xxdup += 1;
                         leave;
                       endif;
                     endif;
                   endif;
                   Reade (K1co#:K1sub#:K1fnd:K1fyr:K1case:K1ndc) pdp02;
                 EndDo;
               endif;
             endif;

           endif;

       endsr;
       //================================================================
       // getast - get the adjudication state
       //================================================================
       begsr getast;

         // get fmcmp/cmast
         xxast = 'UC';
         Chain cmky1 pcm01;
         *in98 = not %found;
         if *in98 = *off;
           kkst = cmast;
         endif;
         Chain stky pst01;
         *in99 = not %found;
         if *in99 = *off;
           xxast = cmast;
         endif;

       endsr;
       //================================================================
       // getEOB - get a new EOB number
       //================================================================
       begsr getEOB;

         xxeob# = *zeros;
         Chain m1ky pm101;
         *in99 = not %found;
         if *in99 = *off;
           m1eob# += 1;
           update pm101;
         endif;
         xxeob# = m1eob#;

       endsr;
       //================================================================
       // getclm - get the claim number
       //================================================================
       begsr getclm;

          if K1PatId <> *Blanks;
            claim# = %Trim(%Subst(K1PatId:6:13));
          else;
            claim# = %editc(K1Fnd:'X') +  %editc(K1fyr:'X') +
                     %editc(K1case:'X');
          endif;

      /end-free
     c                   move      xxwcf#        kkwcf#
     c                   move      xxfyr         kkfyr
     c                   move      xxcase        kkcase

     c                   move      ##co#         #1co#
     c                   move      kkwcf#        #1wcf#

     c                   call      'FA700R'
     c                   parm      *blanks       ##rtcd            1
     c                   parm                    #1co#             3
     c                   parm                    #1wcf#            3
     c                   parm      *blanks       #1sub#            5
     c                   parm      *blanks       #1fnd             3

      /free
         if ##rtcd = *blanks;
      /end-free
     c                   move      #1sub#        kksub#
     c                   move      #1fnd         kkfnd
      /free
         else;
      /end-free
     c                   z-add     *zeros        kksub#
     c                   z-add     *zeros        kkfnd
      /free
         endif;

       endsr;
       //================================================================
       // mainsr - main subroutine
       //================================================================
       begsr mainsr;

      /end-free
     c                   call      comapi        compl                  99
     c                   z-add     0             xxprcd            9 0
     c                   z-add     0             xxercd            9 0
     c                   z-add     0             xxdup             9 0

      /free
         if q1prms > 2;

           Chain(n) @@rrnk1 pk10p;
           *in98 = not %found;
      /end-free
     c  n98cmky1         Chain     pcm01                              98
     c                   movel     fein          K1pfei
     c                   movel     cmast         svast
     c                   movel     K1pfei        kkfein
     c                   eval      kkmod = K1mod
     c  n98pdky2         Chain     ppd01                              98
      /free
           if *in98 = *off;

      /end-free
     c                   move      ##co#         #1co#
     c                   move      cmsub#        #1sub#
     c                   move      cmfnd         #1fnd

     c                   call      'FA700R'
     c                   parm      *blanks       ##rtcd            1
     c                   parm                    #1co#             3
     c                   parm      *blanks       #1wcf#            3
     c                   parm                    #1sub#            5
     c                   parm                    #1fnd             3

      /free
             if ##rtcd = *blanks;
      /end-free
     c                   move      #1wcf#        kkwcf#
      /free
             else;
      /end-free
     c                   z-add     *zeros        kkwcf#
      /free
             endif;

             // skip denials to be processed
             if K1clmtyp <> 'D';

               if K1eob# = *zeros;
                 exsr getEOB;
                 K1eob# = xxeob#;
                 saveob#= xxeob#;
               endif;

               exsr writer;
               exsr updat1;

             endif;

           endif;

         else;

           Setll k1ky1 pk101;
           Reade k1ky1 pk101;
           if not %eof (fak1l1);
             Dow not %eof (fak1l1);

               // when recid = 5 i.e. eob info
               if K1recid = '5';

                 // skip denials to be processed
                 if K1clmtyp <> 'D';

                   exsr errck;
                   exsr updt1a;

                   if inderr = *off;
                     exsr writer;
                     exsr updt1b;
                   endif;

                 endif;

               endif;

               Reade k1ky1 pk101;
               if %eof (fak1l1);
                 exsr updat9;
               endif;
             EndDo;
           else;
             exsr updat9;
           endif;
         endif;

         *inlr = *on;

       endsr;

       //================================================================
       // updt1a - update pharmacy record (fak1p) - 1st update
       //================================================================
       begsr updt1a;

         K1pfei = %dec(fein:9:0);
         if inderr = *on;

           select;
           when inddup = *on;
            K1prfl = 'D';
           other;
            K1prfl = 'E';
           endsl;

      /end-free
     c                   move      *blanks       K1dlus
     c                   move      *blanks       K1dlpg
     c                   z-add     0             K1dltm
     c                   z-add     0             K1dldt
     c                   move      *blanks       K1dlre
     c                   move      *blanks       K1dlus
     c                   move      *blanks       K1prus
     c                   z-add     0             K1prdt
     c                   z-add     0             K1prtm
     c                   z-add     0             K1uldt

      /free
         else;

           if K1eob# = *zeros;
             exsr getEOB;
             K1eob# = xxeob#;
           endif;

      /end-free
     c                   z-add     cmco#         K1co#
     c                   z-add     cmsub#        K1sub#
     c                   z-add     cmfnd         K1fnd
     c                   z-add     cmfyr         K1fyr
     c                   z-add     cmcase        K1case
     c                   movel(p)  cmss#         K1pss#

      /free
         endif;

         // initialize the fields

         update pk101;

       endsr;
       //================================================================
       // updt1b - update hes bill header record (fak1p) - 2nd update
       //================================================================
       begsr updt1b;

        Chain k1ky2 pk104;
        *in99 = not %found;
        if *in99 = *off;
      /end-free
     c                   time                    dsdttm
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         K1prdt

     c                   movel     *blanks       K1dlus
     c                   movel     *blanks       K1dlpg
     c                   z-add     *zeros        K1dltm
     c                   z-add     *zeros        K1dldt
     c                   movel     *blanks       K1dlre
     c                   movel(p)  'Y'           K1prfl

      /free
         update pk104;
         endif;

       endsr;
       //================================================================
       // updat1 - update pharmacy bill header record (fak1p) - single  mode
       //================================================================
       begsr updat1;
         Chain @@rrnk1 pk10p;
          K1eob#= saveob#;
         *in98 = not %found;
         if *in98 = *off;
      /end-free
     c                   movel(p)  'Y'           K1prfl
     c                   time                    dsdttm
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         K1prdt

      /free
           update pk10p;
         endif;

       endsr;
       //================================================================
       // updat9 - update pharmacy bill transmission file (fak9p).
       //================================================================
       begsr updat9;

         //Chain(n) k1ky1 pk101;
         //*in99 = not %found;
         //if *in99 = *on;
           Chain @@bch pk901;
           *in99 = not %found;
           if *in99 = *off;

      /end-free
     c                   movel(p)  q1user        k9user
     c                   movel(p)  q1pgm         k9pgm
     c                   time                    dsdttm
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         k9prdt
     c                   movel     srdto         k9dt
     c                   z-add     dstime        k9prtm

     c                   add       xxprcd        k9trct
     c                   z-add     xxercd        k9err
     c                   add       xxdup         k9dup

      /free
             update pk901;
           endif;

         //endif;

       endsr;
       //================================================================
       // writer - write data base record and after processing
       //================================================================
       begsr writer;
         xxprcd += 1;

         // get the adjudication state
         exsr getast;

         kkfein = %dec(fein:9:0);
         Chain kkfein ppd01;

         // write the header records
         exsr wrteop;
         exsr wrteap;
         exsr wrtesp;

         select;
         when eobtyp = 'X';
           exsr wrtepp;
         endsl;

         // write the detail records

         exsr wrtdtl;
         if eaplnk = '2'
         or eaplnk = '3';

           // write the check request unless the claim is denied

           if inddup <> *on;
             if eapaid <> 0;
               exsr wrtrp1;
             endif;
           endif;
         endif;

       endsr;
       //================================================================
       // wrteap - write faeap record
       //================================================================
       begsr wrteap;

         Chain eaky2 pea01;
         *in99 = not %found;
         if *in99 = *on;

           clear pea01;
      /end-free
     c                   z-add     eoco#         eaco#
     c                   z-add     eosub#        easub#
     c                   z-add     eofnd         eafnd
     c                   z-add     eofyr         eafyr
     c                   z-add     eocase        eacase
     c                   movel     eoss#         eass#
     c                   z-add     eoacdt        eaacdt
     c                   z-add     eoeob#        eaeob#
     c                   z-add     eorcn         earcn
     c                   z-add     eotrdt        eatrdt

     c                   z-add     eochrg        eachrg
     c                   z-add     eomra         eamra
     c                   z-add     eoothr        eaothr
     c                   z-add     eondis        eandis
     c                   z-add     eopaid        eapaid
     c                   movel     eosys         easys
     c                   eval      easinv = eosinv

     c                   movel(p)  'A'           eastat
     c                   movel(p)  eouser        eauser
     c                   movel(p)  eopgm         eapgm
     c                   z-add     eotime        eatime
     c                   z-add     eodt          eadt
     c                   movel(p)  eouser        eaiusr
     c                   movel(p)  eopgm         eaipgm
     c                   z-add     eoitim        eaitim
     c                   z-add     eoidt         eaidt

      /free
           if eoblst <> 'P';
             Chain(n) f1ky pf101;
             *in99 = not %found;
             if *in99 = *off;
               if f1outa = 'Y';
                 eaplnk = '1';
               else;
                 if eopaid <= 0;
                   eaplnk = '2';
                 else;
                   eaplnk = '3';
                 endif;
               endif;
             endif;
           endif;

      /end-free
     c                   z-add     eofein        eafein
     c                   z-add     eomod         eamod

      /free
           if pddba1 <> 'Y';
             if pdfsn = *blanks;
               eapnam = pdname;
             else;
      /end-free
     c     pdfsn         cat(p)    pdname:1      eapnam
      /free
             endif;
      /end-free
     c                   move      pdadr1        eaadr1
     c                   move      pdadr2        eaadr2
      /free
           else;
      /end-free
     c                   move      pdadr1        eapnam
     c                   move      pdadr2        eaadr1
     c                   move      *blanks       eaadr2
      /free
           endif;
      /end-free
     c                   move      pdcty         eacty
     c                   move      pdst          east
     c                   move      pdzip         eazip

     c                   movel(p)  'N'           eapnwc

     c                   movel     'N'           eapbrc

      /free
           write pea01;
         endif;

       endsr;
       //================================================================
       // wrteop - write faeop record
       //================================================================
       begsr wrteop;

      /end-free
     c                   time                    dsdttm

      // write first eomb

      /free
         Chain eobky peo01;
         *in99 = not %found;
         if *in99 = *on;

           clear peo01;
      /end-free
     c                   z-add     K1co#         eoco#
     c                   z-add     K1fnd         eofnd
     c                   z-add     K1fyr         eofyr
     c                   z-add     K1case        eocase

     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         eotrdt

     c                   z-add     cmacdt        eoacdt

     c                   movel     fein          eofein
     c                   movel     '000'         eomod
     c                   movel     K1fdat        xxdate            8 0

     c                   z-add     xxdate        srdti
     c                   call      'SRCVTDAT'    dtcym2
     c                   movel     srdto         eofrdt

     c                   movel     K1fdat        xxdate
     c                   z-add     xxdate        srdti
     c                   call      'SRCVTDAT'    dtcym2
     c                   movel     srdto         eotodt
     c                   eval      eosinv = K1clm#

      /free
           if pddba1 <> 'Y';
             if pdfsn = *blanks;
      /end-free
     c                   move      pdname        eopnam
      /free
             else;
      /end-free
     c     pdfsn         cat(p)    pdname:1      eopnam
      /free
             endif;
      /end-free
     c                   move      pdadr1        eoadr1
     c                   move      pdadr2        eoadr2
      /free
           else;
      /end-free
     c                   move      pdadr1        eopnam
     c                   move      pdadr2        eoadr1
     c                   move      *blanks       eoadr2
      /free
           endif;
      /end-free
     c                   move      pdcty         eocty
     c                   move      pdst          eost
     c                   move      pdzip         eozip

     c                   movel(p)  xxast         eoast
     c                   z-add     K1eob#        eochk#
     c                   z-add     K1eob#        eoeob#

     c                   movel(p)  'P'           eosys

     c                   eval      eouser = 'PERFORMRX'
     c                   movel(p)  q1pgm         eopgm
     c                   z-add     dstime        eotime
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         eodt
     c                   eval      eoiusr = 'PERFORMRX'
     c                   movel     srdto         eoidt
     c                   movel(p)  'N'           eotape
     c                   movel     eozip         xxzip             5
     c                   movel     xxzip         eoflic
     c                   z-add     1             eo#bll
     c                   movel     xxast         eoast
     c                   z-add     K1sub#        eosub#

     c                   movel(p)  xxzip         kkzipa
      /free
           setgt gzky pgz01;
           readp pgz01;
           *in99 = %eof;
           if *in99 = *off
           and kkzipa <= gztozp;
      /end-free
     c                   move      gzzipa        eozipa
      /free
           else;
      /end-free
     c                   move      *blanks       eozipa
      /free
           endif;

      /end-free
     c                   movel(p)  K1pss#        eoss#

     c                   movel(p)  q1pgm         eoipgm
     c                   z-add     dstime        eoitim

     c                   movel(p)  'X'           eobtyp

     c                   movel(p)  'C'           eoblst

      // if the bill is a possible duplicate payment pend the bill and
      // set the upload flag = D

      /free
           if inddup = *on
           or K1prfl = 'D';
      /end-free
     c                   movel     'P'           eoblst
      /free
           endif;
      /end-free
     c                   movel     '2'           eocscd

     c                   z-add     0             eocsdt

     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         eocrdt
      /free
           select;
            when K1clmtyp = 'A';
         //*** Beg Del *** 09/09/16 ***************************
         //**if K1adjdel = 'D';
         //*** End Del *** 09/09/16 ***************************
         //*** Beg Chg *** 09/09/16 ***************************
         //*** eochrg = convrtamt(K1sincospd) + convrtamt(K1scoay) +
         //***          convrtamt(K1sdispf) + convrtamt(K1saltaxs);
               eochrg = convrtamt(K1chrg);
         //*** End Chg *** 09/09/16 ***************************
         //*** Beg Chg *** 09/09/16 ***************************
         //*** eopaid = convrtamt(K1amtadj);
               eopaid = convrtamt(K1paid);
         //*** End Chg *** 09/09/16 ***************************
               eondis = eochrg - eopaid;
               eomra  = 0;
         //*** Beg Del *** 09/09/16 ***************************
         //**endif;
         //*** End Del *** 09/09/16 ***************************
            when K1clmtyp = 'D';
         //*** Beg Chg *** 09/09/16 ***************************
         //**eochrg = convrtamt(K1sincospd) + convrtamt(K1scoay) +
         //***          convrtamt(K1sdispf) + convrtamt(K1saltaxs);
             eochrg = convrtamt(K1chrg);
         //*** End Chg *** 09/09/16 ***************************
             eopaid = 0;
             eondis = convrtamt(K1AmtRej);

             eomra  = 0;
            when K1clmtyp = 'P';
         //*** Beg Chg *** 09/09/16 ***************************
         //**eochrg = convrtamt(K1sincospd) + convrtamt(K1scoay) +
         //**           convrtamt(K1sdispf) + convrtamt(K1saltaxs);
             eochrg = convrtamt(K1chrg);
         //*** End Chg *** 09/09/16 ***************************
             eopaid = convrtamt(K1paid);
             eondis = eochrg - eopaid;
             eomra  = 0;
           endsl;
      /end-free


     c                   movel(p)  'PERRX'       eontwk


     c                   movel     eoeob1        eospcd

     c                   movel(p)  K1pss#        kkss#
     c                   movel(p)  cmlsn         eolsn
     c                   movel(p)  cmfsn         eofsn
     c                   movel     cmmid         eomid

      /free
           write peo01;
           Chain(n) eobky peo01;
           *in99 = not %found;
         else;

           update peo01;

         endif;

       endsr;
       //================================================================
       // wrtesp - write faesp record
       //================================================================
       begsr wrtesp;

         Chain esky pes01;
         *in99 = not %found;
         if *in99 = *on;

           clear pes01;
      /end-free
     c                   z-add     eoco#         esco#
     c                   z-add     eosub#        essub#
     c                   z-add     eofnd         esfnd
     c                   z-add     eofyr         esfyr
     c                   z-add     eocase        escase
     c                   z-add     eoeob#        eseob#
     c                   z-add     eorcn         esrcn
     c                   movel     eoss#         esss#
     c                   z-add     eoacdt        esacdt

      // change logic in case bill is being pended

     c                   movel(p)  eoblst        esblst
      /free
           if eoblst = 'P';
      /end-free
     c                   movel(p)  'DNY'         esstcd
      /free
           endif;
      /end-free
     c                   movel(p)  reas1         esreas

     c                   movel     'A'           esstat
     c                   movel(p)  eouser        esuser
     c                   movel(p)  eopgm         espgm
     c                   z-add     eotime        estime
     c                   z-add     eodt          esdt
     c                   movel(p)  eouser        esiusr
     c                   movel(p)  eopgm         esipgm
     c                   z-add     eoidt         esidt
     c                   z-add     eotime        esitim

      /free
           write pes01;
         endif;

       endsr;
       //================================================================
       // wrtcap - write fdcap record
       //================================================================
       begsr wrtcap;

      /end-free
     c                   z-add     *hival        kkdseq
      /free
         setgt caky pca02;
         readpe caky2 pca02;
         *in99 = %eof;
         if *in99 = *off;
      /end-free
     c     cadseq        add       1             xxdseq
      /free
         else;
      /end-free
     c                   z-add     1             xxdseq
      /free
         endif;

         clear pca01;
      /end-free
     c                   z-add     eoco#         caco#
     c                   z-add     eofnd         cafnd
     c                   z-add     eofyr         cafyr
     c                   z-add     eocase        cacase
     c                   movel     '2'           cacscd
      /free
         if cmstat = 'C';
      /end-free
     c                   movel     'P'           caflag
      /free
         endif;

      /end-free
     c                   movel     K1fdat        xxdate
     c                   z-add     xxdate        srdti
     c                   call      'SRCVTDAT'    dtcym2
     c                   movel     srdto         cafdat
     c                   movel     srdto         catdat

     c                   z-add     eotrdt        cardat
     c                   z-add     eofein        cafein
     c                   z-add     eomod         camod

     c                   movel     '1'           caprty

     c                   z-add     xxdseq        cadseq
     c                   z-add     1             calin#
     c                   z-add     eochk#        cachk#
     c                   z-add     eoeob#        caeob#
     c                   movel     eoss#         cass#
     c                   z-add     eoacdt        caacdt
     c                   movel     'A'           castat

     c                   movel(p)  'X'           carcfl

     c                   movel(p)  eouser        causer
     c                   movel(p)  eopgm         capgm
     c                   z-add     eodt          cadt
     c                   z-add     eotime        catime
     c                   movel(p)  eouser        caiusr
     c                   movel(p)  eopgm         caipgm
     c                   z-add     eoidt         caidt
     c                   z-add     eoitim        caitim

     c                   z-add     eosub#        casub#
     c                   move      eozipa        cazipa
     c                   movel(p)  eoast         caast

      /free
      /end-free
     c                   move      1             caunts
      /free
         //***** Beg Add *** 02/06/17 ***************************
          // Load Units from Plan Metric Decimal Quantity
          xxamt3 = convrtamt3(K1mqty);
          CAunts = xxamt3;
         //***** End Add *** 02/06/17 ***************************

           select;
            when K1clmtyp = 'A';
         //*** Beg Del *** 09/09/16 ***************************
         //**if K1adjdel = 'D';
         //*** End Del *** 09/09/16 ***************************
         //*** Beg Chg *** 09/09/16 ***************************
         //*** cachrg = convrtamt(K1sincospd) + convrtamt(K1scoay) +
         //***          convrtamt(K1sdispf) + convrtamt(K1saltaxs);
               cachrg = convrtamt(K1chrg);
         //*** End Chg *** 09/09/16 ***************************
         //*** Beg Chg *** 09/09/16 ***************************
         //*** capaid = convrtamt(K1amtadj);
               capaid = convrtamt(K1paid);
         //*** End Chg *** 09/09/16 ***************************
               candis = cachrg - capaid;
               camraa = capaid;
               camra  = 0;
         //*** Beg Del *** 09/09/16 ***************************
         //**endif;
         //*** End Del *** 09/09/16 ***************************
            when K1clmtyp = 'D';
         //*** Beg Chg *** 09/09/16 ***************************
         //**cachrg = convrtamt(K1sincospd) + convrtamt(K1scoay) +
         //**           convrtamt(K1sdispf) + convrtamt(K1saltaxs);
             cachrg = convrtamt(K1chrg);
         //*** End Chg *** 09/09/16 ***************************
             capaid = 0;
             candis = convrtamt(K1AmtRej);
             camraa = capaid;
             camra  = 0;
            when K1clmtyp = 'P';
         //*** Beg Chg *** 09/09/16 ***************************
         //**cachrg = convrtamt(K1sincospd) + convrtamt(K1scoay) +
         //***          convrtamt(K1sdispf) + convrtamt(K1saltaxs);
             cachrg = convrtamt(K1chrg);
         //*** End Chg *** 09/09/16 ***************************
             capaid = convrtamt(K1paid);
             candis = cachrg - capaid;
             camraa = capaid;
             camra  = 0;
           endsl;

           catax = convrtamt(K1saltaxp);

      /end-free

     c                   z-add     eorcn         carcn

     c                   movel(p)  K1ndc         cacode
     c                   movel(p)  K1ndc         cacpt4
     c                   movel(p)  K1ndc         cabprc

      /free
         exsr wrtdap;

         write pca01;

       endsr;
       //================================================================
       // wrtdap - write fddap record
       //================================================================
       begsr wrtdap;

      /end-free
     c                   z-add     caeob#        kkeob#
     c                   z-add     cadseq        kkdseq

      /free
         Chain daky pda01;
         *in99 = not %found;
         if *in99 = *on;
           clear pda01;
      /end-free
     c                   z-add     caco#         daco#
     c                   z-add     casub#        dasub#
     c                   z-add     cafnd         dafnd
     c                   z-add     cafyr         dafyr
     c                   z-add     cacase        dacase
     c                   movel     cass#         dass#
     c                   z-add     caacdt        daacdt
     c                   z-add     caeob#        daeob#
     c                   z-add     eorcn         darcn#
     c                   z-add     cadseq        dadseq
     c                   z-add     cardat        datrdt
     c                   move      cachrg        dachrg
     c                   z-add     camra         damra
     c     caothr        sub       odothr        daothr
     c     candis        sub       odndis        dandis
     c                   move      capaid        dapaid

     c                   movel     'A'           dastat
     c                   movel(p)  causer        dauser
     c                   movel(p)  capgm         dapgm
     c                   z-add     catime        datime
     c                   z-add     cadt          dadt
     c                   movel(p)  causer        daiusr
     c                   movel(p)  capgm         daipgm
     c                   z-add     caitim        daitim
     c                   z-add     caidt         daidt

      /free
           write pda01;
         endif;

       endsr;
       //================================================================
       // wrtdtl - write detail records
       //================================================================
       begsr wrtdtl;

         exsr wrtcap;
         exsr wrtdep;

         select;
         when eobtyp = 'X';
           exsr wrtdpp;
         endsl;

       endsr;
       //================================================================
       // wrtrp1 - write warpp record  - provider check
       //================================================================
       begsr wrtrp1;

         Chain(n) eaky2 pea01;
         *in99 = not %found;

      /end-free
     c                   z-add     eoeob#        kkeob#
     c                   z-add     eorcn         kkeobs
      /free
         Chain rpky1 prp01;
         *in99 = not %found;
         if *in99 = *on;

           setgt rpky2 prp02;
           readpe rpky2 prp02;
           *in99 = %eof;
           if *in99 = *off;
      /end-free
     c     rpseq#        add       1             xxseq#
      /free
           else;
      /end-free
     c                   z-add     1             xxseq#
      /free
           endif;

           clear prp01;
      /end-free
     c                   z-add     eoco#         rpco#
     c                   z-add     kkwcf#        rpfnd
     c                   z-add     eofyr         rpfyr
     c                   z-add     eocase        rpcase
     c                   z-add     xxseq#        rpseq#
     c                   z-add     eotrdt        rptrdt
     c                   z-add     eofein        rpfein
     c                   z-add     eomod         rpmod
     c                   z-add     eapaid        rpamt
     c                   movel     'G'           rppgtp
     c                   eval      rprmds = eosinv

      /free
           Chain cmky3 pcm02;
           *in99 = not %found;
      /end-free
     c                   movel     'P'           kkptyp
     c                   movel     eosys         kksys
      /free
           Chain ciky pci01;
           *in99 = not %found;
           if *in99 = *off;

             if cmcvfl = 'Y'
             and eobtyp <> 'X';
      /end-free
     c                   move(p)   cip1cv        rpbr2
      /free
             else;
               if eobtyp = 'X';
      /end-free
     c                   move(p)   cixbr2        rpbr2
      /free
               else;
      /end-free
     c                   move(p)   cibr2         rpbr2
      /free
               endif;
             endif;
             if cmltfl = 'I';
      /end-free
     c                   move(p)   cibr7l        rpbr7
      /free
             else;
      /end-free
     c                   move(p)   cibr7n        rpbr7
      /free
             endif;

      /end-free
     c                   movel(p)  cists         rpsts

      /free
           else;
             if eosys = 'P';
               if eobtyp = 'X';
      /end-free
     c                   move(p)   '01'          rpbr2
      /free
               else;
      /end-free
     c                   movel(p)  'MD'          rpbr2
      /free
               endif;
             else;
      /end-free
     c                   movel(p)  'HP'          rpbr2
      /free
             endif;
             if cmltfl = 'i';
      /end-free
     c                   movel(p)  'LT'          rpbr7
      /free
             else;
      /end-free
     c                   movel(p)  'NL'          rpbr7
      /free
             endif;
      /end-free
     c                   movel(p)  'PD'          rpsts
      /free
           endif;
      /end-free
     c                   movel(p)  cwncci        rpncci
     c                   z-add     eofrdt        rpfrdt
     c                   z-add     eotodt        rptodt
     c                   movel(p)  'MP'          rptrcd
     c                   movel(p)  'P'           rptype
     c                   movel(p)  'M'           rpretp
     c*** Beg Chg *** 09/09/16 ********************************
     c**************     movel(p)  'M '          rprecd
     c                   movel(p)  'ME'          rprecd
     c*** End Chg *** 09/09/16 ********************************
     c                   movel(p)  rptrcd        kkkey
      /free
           Chain tbky ppc01;
           *in99 = not %found;
           if *in99 = *off;
      /end-free
     c                   movel(p)  pcdesc        rpdesc
      /free
           endif;
      /end-free
     c                   movel(p)  eopnam        rppnam
     c                   movel(p)  eoadr1        rpadr1
     c                   movel(p)  eoadr2        rpadr2
     c                   movel(p)  eocty         rpcty
     c                   movel(p)  eost          rpst
     c                   z-add     eozip         rpzip
     c                   movel     'P'           rpstat
     c                   movel(p)  eouser        rpuser
     c                   movel(p)  eopgm         rppgm
     c                   z-add     eotime        rptime
     c                   z-add     eodt          rpdt
     c                   z-add     eoeob#        rpeob#
     c                   z-add     eorcn         rpeobs
     c                   z-add     eodt          rprcvd
     c                   movel(p)  eosinv        rpsinv
     c                   z-add     eocsdt        rpcsdt
     c                   movel(p)  rpsts         kkkey
      /free
           Chain tbky pps01;
           *in99 = not %found;
           if *in99 = *off;
      /end-free
     c                   move(p)   pssrt         rpsort
      /free
           endif;

      /end-free
     c                   movel(p)  'N'           rpsent
     c                   movel(p)  'ANC'         rprqst

      //  call api to determine the bank "code".

     c                   move      rpco#         $$co#
     c                   move      rpfnd         $$fnd
     c                   move      rpfyr         $$fyr
     c                   move      rpcase        $$case
     c                   z-add     rptrdt        qxtrdt
     c                   z-add     rpfrdt        qxfrdt
     c                   z-add     rptodt        qxtodt
     c                   movel     rptrcd        qxtrcd
     c                   movel     rprecd        qxrecd
     c                   movel     rpbr2         qxpv1
     c                   movel     rpbr7         qxpv2
     c                   eval      qxseq# = RPcmsq
     c                   eval      qxeob# = %char(RPeob#)
     c                   eval      qxfein = RPfein
     c                   eval      qxmod = RPmod
     c                   eval      qxdsq# = RPdsq#
      //
     c                   call      'WA023R1'
     c                   parm      '2'           $$mode            1
     c                   parm                    $$co#             3
     c                   parm                    $$fnd             3
     c                   parm                    $$fyr             3
     c                   parm                    $$case            7
     c                   parm      'WC'          $$appl            2
     c                   parm      data          $$data           75
     c                   parm      *blanks       $$cktp           10
     c                   parm      *blanks       $$rtcd            1

     c                   movel(p)  $$cktp        rpcktp

     c                   movel(p)  eaiusr        rpiusr
     c                   movel(p)  eapgm         rpipgm
     c                   z-add     eaitim        rpitm
     c                   z-add     eaidt         rpidt

      /free
           if rpamt <> *zeros;
             write prp01;
           endif;
         endif;

       endsr;
       //================================================================
       // wrtdpp - write fddpp record
       //================================================================
       begsr wrtdpp;

      /end-free
     c                   z-add     caeob#        kkeob#
     c                   z-add     cadseq        kkdseq
      /free
         Chain capky1 pdp01;
         *in99 = not %found;
         if *in99 = *on;

           clear pdp01;
      /end-free
     c                   z-add     caco#         dpco#
     c                   z-add     casub#        dpsub#
     c                   z-add     cafnd         dpfnd
     c                   z-add     cafyr         dpfyr
     c                   z-add     cacase        dpcase
     c                   movel     cass#         dpss#
     c                   z-add     caacdt        dpacdt
     c                   z-add     caeob#        dpeob#
     c                   z-add     cadseq        dpdseq
     c                   movel(p)  ' '           dpnwrf
     c                   movel(p)  K1ndc         dpbndc
     c                   movel(p)  K1ndc         dpndc
     c                   movel(p)  K1dname       dpdesc

      /free
        monitor;
         dprxdy = %dec(K1days:5:0);
        on-error;
         dprxdy = *zeros;
        endmon;

        xxamt4 = convrtamt4(K1mqty);
        DPrxmq = xxamt4;
        DPdfee = convrtamt(K1dispf);
        DPrx#  = K1rx#;
        DPcert = K1sdaw;
        DPbcdt = K1bcdet;

      /end-free
     c****               movel     K1rxgb        dprxgb

     c                   move      K1fdat        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         dprxfd

     c                   z-add     1             dpunts
     c***** Beg Add *** 02/06/17 *************************************
      /free
          // Load Units from Plan Metric Decimal Quantity (loaded in CAunts)
          DPunts = CAunts;
      /end-free
     c***** End Add *** 02/06/17 *************************************

     c                   movel     'A'           dpstat
     c                   movel(p)  causer        dpuser
     c                   movel(p)  capgm         dppgm
     c                   z-add     catime        dptime
     c                   z-add     cadt          dpdt
     c                   movel(p)  causer        dpiusr
     c                   movel(p)  capgm         dpipgm
     c                   z-add     caitim        dpitim
     c                   z-add     caidt         dpidt

      /free
           write pdp01;
         endif;

       endsr;
       //================================================================
       // wrtepp - write faepp record
       //================================================================
       begsr wrtepp;

      /end-free
     c                   z-add     eoeob#        kkeob#
      /free
         Chain eobky1 pep01;
         *in99 = not %found;
         if *in99 = *on;

           clear pep01;
      /end-free
     c                   z-add     eoco#         epco#
     c                   z-add     eosub#        epsub#
     c                   z-add     eofnd         epfnd
     c                   z-add     eofyr         epfyr
     c                   z-add     eocase        epcase
     c                   z-add     eoeob#        epeob#
     c                   movel(p)  eoss#         epss#
     c                   z-add     eoacdt        epacdt
     c                   movel     'N'           epedi
     c****               movel(p)  K1phnm        epphnm
     c****               movel(p)  K1pnpi        eprfnp
     c****               movel(p)  K1pnam        eppnam

     c****               movel(p)  K1pdea        eppdea

     c                   movel(p)  eouser        epuser
     c                   movel(p)  eopgm         eppgm
     c                   z-add     eotime        eptime
     c                   z-add     eodt          epdt
     c                   movel(p)  eouser        epiusr
     c                   movel(p)  eopgm         epipgm
     c                   z-add     eoitim        epitim
     c                   z-add     eoidt         epidt

      /free
           write pep01;
         endif;

       endsr;
       //================================================================
       // wrtde1 - write fddep record
       //================================================================
       begsr wrtdep;

         // look for the three codes
         xxadjseq = 1;
         Dow xxadjseq <= 3;

           select;
            when xxadjseq = 1;
             xxadjcd = K1adj1;
            when xxadjseq = 2;
             xxadjcd = K1adj2;
            when xxadjseq = 3;
             xxadjcd = K1adj3;
           other;
             leave;
           endsl;

           Chain(n) xxadjcd ppr01;
           if %found(ftprxl1);

      /end-free
     c                   z-add     *hival        kkseq#
      /free
             setgt deky2 pde02;
             readpe deky1 pde02;
             *in99 = %eof;
             if *in99 = *off;
      /end-free
     c     deseq#        add       1             hdseq#
      /free
             else;
      /end-free
     c                   z-add     1             hdseq#
      /free
             endif;

             clear pde01;
      /end-free
     c                   z-add     caco#         deco#
     c                   z-add     casub#        desub#
     c                   z-add     cafnd         defnd
     c                   z-add     cafyr         defyr
     c                   z-add     cacase        decase
     c                   movel     cass#         dess#
     c                   z-add     caacdt        deacdt
     c                   z-add     caeob#        deeob#
     c                   z-add     cadseq        dedseq
     c                   z-add     hdseq#        deseq#

     c                   movel     'A'           destat
     c                   movel(p)  causer        deuser
     c                   movel(p)  capgm         depgm
     c                   z-add     catime        detime
     c                   z-add     cadt          dedt
     c                   movel(p)  causer        deiusr
     c                   movel(p)  capgm         deipgm
     c                   z-add     caitim        deitim
     c                   z-add     caidt         deidt
     c                   movel(p)  'U'           deesrc
     c                   z-add     eorcn         dercn
     c                   movel(p)  'Y'           deprnt
      /free
             deeob = xxadjcd;
             denot1 = %subst(prxdesc:1:70);
             denot2 = %subst(prxdesc:71:70);
             denot3 = %subst(prxdesc:141:10);

             write pde01;
           endif;
           xxadjseq += 1;
         EndDo;

       endsr;
      /end-free

     p* procedure implementations
     p*====================================================================
     p*  convrtamt - convert coded value to amount value
     p*====================================================================
     p convrtamt       b                   export
     d convrtamt       pi             9  2
     d inamt                          8
     d*
     d* standalone variables
     d outamt          s              9  2 inz
     d zzamt           s              9    inz
     d xxcode          s              1    inz
     d*
      /free

       outamt = *zeros;
       xxcode = %subst(inamt:8:1);
       zzamt = %subst(inamt:1:6) + '.' + %subst(inamt:7:2);
       select;

        when xxcode = '{';
         %subst(zzamt:9:1) = '0';
         monitor;
          outamt = %dec(zzamt:9:2);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = '}';
         %subst(zzamt:9:1) = '0';
         monitor;
          outamt = %dec(zzamt:9:2);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'A';
         %subst(zzamt:9:1) = '1';
         monitor;
          outamt = %dec(zzamt:9:2);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'J';
         %subst(zzamt:9:1) = '1';
         monitor;
          outamt = %dec(zzamt:9:2);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'B';
         %subst(zzamt:9:1) = '2';
         monitor;
          outamt = %dec(zzamt:9:2);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'K';
         %subst(zzamt:9:1) = '2';
         monitor;
          outamt = %dec(zzamt:9:2);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'C';
         %subst(zzamt:9:1) = '3';
         monitor;
          outamt = %dec(zzamt:9:2);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'L';
         %subst(zzamt:9:1) = '3';
         monitor;
          outamt = %dec(zzamt:9:2);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'D';
         %subst(zzamt:9:1) = '4';
         monitor;
          outamt = %dec(zzamt:9:2);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'M';
         %subst(zzamt:9:1) = '4';
         monitor;
          outamt = %dec(zzamt:9:2);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'E';
         %subst(zzamt:9:1) = '5';
         monitor;
          outamt = %dec(zzamt:9:2);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'N';
         %subst(zzamt:9:1) = '5';
         monitor;
          outamt = %dec(zzamt:9:2);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'F';
         %subst(zzamt:9:1) = '6';
         monitor;
          outamt = %dec(zzamt:9:2);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'O';
         %subst(zzamt:9:1) = '6';
         monitor;
          outamt = %dec(zzamt:9:2);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'G';
         %subst(zzamt:9:1) = '7';
         monitor;
          outamt = %dec(zzamt:9:2);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'P';
         %subst(zzamt:9:1) = '7';
         monitor;
          outamt = %dec(zzamt:9:2);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'H';
         %subst(zzamt:9:1) = '8';
         monitor;
          outamt = %dec(zzamt:9:2);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'Q';
         %subst(zzamt:9:1) = '8';
         monitor;
          outamt = %dec(zzamt:9:2);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'I';
         %subst(zzamt:9:1) = '9';
         monitor;
          outamt = %dec(zzamt:9:2);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'R';
         %subst(zzamt:9:1) = '9';
         monitor;
          outamt = %dec(zzamt:9:2);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

       other;
       endsl;

       return outamt;

      /end-free
     p convrtamt       e

     p***** Beg Add *** 02/06/17 ***************************************
     p*====================================================================
     p*  convrtamt3 - convert coded value to amount value (3 decimal place)
     p*====================================================================
     p convrtamt3      b                   export
     d convrtamt3      pi            11  3
     d inamt                         10
     d*
     d* standalone variables
     d outamt          s             11  3 inz
     d zzamt           s             11    inz
     d xxcode          s              1    inz
     d*
      /free

       outamt = *zeros;
       xxcode = %subst(inamt:10:1);
       zzamt = %subst(inamt:1:7) + '.' + %subst(inamt:8:3);
       select;

        when xxcode = '{';
         %subst(zzamt:11:1) = '0';
         monitor;
          outamt = %dec(zzamt:11:3);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = '}';
         %subst(zzamt:11:1) = '0';
         monitor;
          outamt = %dec(zzamt:11:3);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'A';
         %subst(zzamt:11:1) = '1';
         monitor;
          outamt = %dec(zzamt:11:3);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'J';
         %subst(zzamt:11:1) = '1';
         monitor;
          outamt = %dec(zzamt:11:3);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'B';
         %subst(zzamt:11:1) = '2';
         monitor;
          outamt = %dec(zzamt:11:3);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'K';
         %subst(zzamt:11:1) = '2';
         monitor;
          outamt = %dec(zzamt:11:3);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'C';
         %subst(zzamt:11:1) = '3';
         monitor;
          outamt = %dec(zzamt:11:3);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'L';
         %subst(zzamt:11:1) = '3';
         monitor;
          outamt = %dec(zzamt:11:3);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'D';
         %subst(zzamt:11:1) = '4';
         monitor;
          outamt = %dec(zzamt:11:3);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'M';
         %subst(zzamt:11:1) = '4';
         monitor;
          outamt = %dec(zzamt:11:3);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'E';
         %subst(zzamt:11:1) = '5';
         monitor;
          outamt = %dec(zzamt:11:3);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'N';
         %subst(zzamt:11:1) = '5';
         monitor;
          outamt = %dec(zzamt:11:3);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'F';
         %subst(zzamt:11:1) = '6';
         monitor;
          outamt = %dec(zzamt:11:3);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'O';
         %subst(zzamt:11:1) = '6';
         monitor;
          outamt = %dec(zzamt:11:3);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'G';
         %subst(zzamt:11:1) = '7';
         monitor;
          outamt = %dec(zzamt:11:3);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'P';
         %subst(zzamt:11:1) = '7';
         monitor;
          outamt = %dec(zzamt:11:3);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'H';
         %subst(zzamt:11:1) = '8';
         monitor;
          outamt = %dec(zzamt:11:3);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'Q';
         %subst(zzamt:11:1) = '8';
         monitor;
          outamt = %dec(zzamt:11:3);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'I';
         %subst(zzamt:11:1) = '9';
         monitor;
          outamt = %dec(zzamt:11:3);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'R';
         %subst(zzamt:11:1) = '9';
         monitor;
          outamt = %dec(zzamt:11:3);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

       other;
       endsl;

       return outamt;

      /end-free
     p convrtamt3      e
     p***** End Add *** 02/06/17 ***************************************

     p*====================================================================
     p*  convrtamt4 - convert coded value to amount value (4 decimal place)
     p*====================================================================
     p convrtamt4      b                   export
     d convrtamt4      pi            11  4
     d inamt                         10
     d*
     d* standalone variables
     d outamt          s             11  4 inz
     d zzamt           s             11    inz
     d xxcode          s              1    inz
     d*
      /free

       outamt = *zeros;
       xxcode = %subst(inamt:10:1);
       zzamt = %subst(inamt:1:6) + '.' + %subst(inamt:7:4);
       select;

        when xxcode = '{';
         %subst(zzamt:11:1) = '0';
         monitor;
          outamt = %dec(zzamt:11:4);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = '}';
         %subst(zzamt:11:1) = '0';
         monitor;
          outamt = %dec(zzamt:11:4);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'A';
         %subst(zzamt:11:1) = '1';
         monitor;
          outamt = %dec(zzamt:11:4);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'J';
         %subst(zzamt:11:1) = '1';
         monitor;
          outamt = %dec(zzamt:11:4);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'B';
         %subst(zzamt:11:1) = '2';
         monitor;
          outamt = %dec(zzamt:11:4);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'K';
         %subst(zzamt:11:1) = '2';
         monitor;
          outamt = %dec(zzamt:11:4);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'C';
         %subst(zzamt:11:1) = '3';
         monitor;
          outamt = %dec(zzamt:11:4);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'L';
         %subst(zzamt:11:1) = '3';
         monitor;
          outamt = %dec(zzamt:11:4);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'D';
         %subst(zzamt:11:1) = '4';
         monitor;
          outamt = %dec(zzamt:11:4);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'M';
         %subst(zzamt:11:1) = '4';
         monitor;
          outamt = %dec(zzamt:11:4);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'E';
         %subst(zzamt:11:1) = '5';
         monitor;
          outamt = %dec(zzamt:11:4);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'N';
         %subst(zzamt:11:1) = '5';
         monitor;
          outamt = %dec(zzamt:11:4);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'F';
         %subst(zzamt:11:1) = '6';
         monitor;
          outamt = %dec(zzamt:11:4);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'O';
         %subst(zzamt:11:1) = '6';
         monitor;
          outamt = %dec(zzamt:11:4);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'G';
         %subst(zzamt:11:1) = '7';
         monitor;
          outamt = %dec(zzamt:11:4);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'P';
         %subst(zzamt:11:1) = '7';
         monitor;
          outamt = %dec(zzamt:11:4);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'H';
         %subst(zzamt:11:1) = '8';
         monitor;
          outamt = %dec(zzamt:11:4);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'Q';
         %subst(zzamt:11:1) = '8';
         monitor;
          outamt = %dec(zzamt:11:4);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'I';
         %subst(zzamt:11:1) = '9';
         monitor;
          outamt = %dec(zzamt:11:4);
         on-error;
          outamt = *zeros;
         endmon;

        when xxcode = 'R';
         %subst(zzamt:11:1) = '9';
         monitor;
          outamt = %dec(zzamt:11:4);
          if outamt <> *zeros;
            outamt = -1 * outamt;
          endif;
         on-error;
          outamt = *zeros;
         endmon;

       other;
       endsl;

       return outamt;

      /end-free
     p convrtamt4      e
