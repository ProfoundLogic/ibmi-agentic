     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*****************************************************************
     H* This program will be used to calculate Medicare Allowances    *
     H* for physician bills.                                          *
     H*****************************************************************
     H*  CREATED BY:                                                  *
     H*  07/13/10  C:198 Project: 114MWS                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/06/25 C:079 Project: 114MIA                                *
     H*                 PA Discount Algorithm - Use FMZRPP1 for PA    *
     H*                                                               *
     H* 06/21/18 C:079 Project: TX101                                 *
     H*                Add TX to DME subroutine                       *
     H*                                                               *
     H* 01/10/18 C:079 Project: FS538                                 *
     H*                Do not use state fields on or after 1/1/2018   *
     H*                                                               *
     H* 04/04/16 C:212 Project: OK022                                 *
     H*                Load allowance for OK state under DME          *
     H*                                                               *
     H* 01/06/16 C:212 Project: FS493                                 *
     H*                Add processing for OK state under CLAB         *
     H*                                                               *
     H* 05/14/13 C:079 Redmine: 2191                                  *
     H*                Only lookup code with mod 53, 26, or TC        *
     H*                                                               *
     H* 04/30/13 C:284 Project: 147MAS                                *
     H*                Load rate for FL                               *
     H*                                                               *
     H* 02/01/13 C:280 Project: 114MBB                                *
     H*                bring into v13r2 and change copyright          *
     H*                                                               *
     H* 01/24/13 C:277 Project : FS430                                *
     H*                Modify F7=Calculate tool, for 1/1/2013         *
     H*                or later date                                  *
     H*                                                               *
     H* 12/14/11 C:251 Project: 114MBB                                *
     H*                Changed to enable translation of P2 to PA      *
     H*                                                               *
     H* 09/02/11 C:274 Project: FS392                                 *
     H*                Add code for Super Rural for Medicare Ambulance*
     H*                                                               *
     H* 08/16/11 C:274 Project: CT007                                 *
     H*                Changed to load DMEPOS amount when state = CT  *
     H*                                                               *
     H* 06/22/11 C:198 Project: 114MYS                                *
     H*                Changed to enable translation of P1 to PA      *
     H*                                                               *
     H* 04/12/11 C:190 Project: KY019                                 *
     H*                Changed logic for ambulance so that if not get *
     H*                a valid KY zip code, set the region/locality   *
     H*                to 00.                                         *
     H*                                                               *
     H* 10/01/10 C:198 Project: WV009                                 *
     H*                Changed to check for medicare release level    *
     H*                                                               *
     H*****************************************************************
     Ffdcap     if   e             disk
     Ffaeolg    if   e           k disk
     Ffddhl     if   e           k disk
     Ffddul     if   e           k disk
     Ffmzrl     if   e           k disk    usropn
     Ffmacl1    if   e           k disk
     Fftlbl1    if   e           k disk
     Fftrvl1    if   e           k disk
     Ffmrcl     if   e           k disk
     Fftgpl3    if   e           k disk
     Ffmdel1    if   e           k disk
     Ffmdfl1    if   e           k disk
     Ffmaml1    if   e           k disk
     Fftuml     if   e           k disk
     F*** Beg Add *** 10/01/10 *************************************************
     Fftrtl2    if   e           k disk
     F*** End Add *** 10/01/10 *************************************************
     F*
     D psds          esds                  extname(szq1p)
     *
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D                 ds
     D  qqcmd                  1     56
     D  qqst                  40     41
     D*
     Dqca              s             56    dim(2) ctdata perrcd(1)
     Dmod              s              2    dim(4)
     Dkkzip            s                   like(zrfzcd)
     Dkkefdt           s                   like(zrefdt)
     Dk1efdt           s                   like(zrefdt)
     Dkkloc            s                   like(zrrgn#)
     Dkkmod            s                   like(dhmod1)
     Dkkst             s                   like(eoast)
     Dkkcpt4           s                   like(cacpt4)
     Dxxunts           s              5  1
     Dxxamt1           s              9  2
     Dxxamt2           s              9  2
     D xxdme           s              1    inz('N')
     D xxmod1          s              2
     D xxmod2          s              2
     D i               s              1  0
     D j               s              1  0
     D k               s              1  0
     D xxtech          s                   like(camra)
     D xxprof          s                   like(camra)
     D xxmd26          s                   like(umod26)
     D xxmd27          s                   like(umod27)
     D*** Beg Add *** 10/01/10 *************************************************
     D xxtype          s                   like(rttype)
     D xxst            s                   like(rtst)
     D*** End Add *** 10/01/10 *************************************************
     D*** Beg Add *** 09/02/11 *************************************************
     D xxrate          s              9  2
     D*** End Add *** 09/02/11 *************************************************
     C*
     C     *entry        plist
     C                   parm                    @@rrn             9 0
     C                   parm                    @@call            1
     C                   parm                    @@eob             3
     C                   parm                    @@msg             7
     C                   parm                    @@modr            1
     C                   parm                    @@rnd             1
     C                   parm                    @@alw1            9 2
     C                   parm                    @@alw2            9 2
     C                   parm                    @@alw3            9 2
     C                   parm                    @@rtcd            1
     *
     C     dtcymd1       plist
     C                   parm                    srdti1            8 0
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *BLANKS       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     *
     C     eoky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C*
     C     dhky          klist
     C                   kfld                    caco#
     C                   kfld                    casub#
     C                   kfld                    cafnd
     C                   kfld                    cafyr
     C                   kfld                    cacase
     C                   kfld                    caeob#
     C                   kfld                    cadseq
     C*
     C     zrky          klist
     C                   kfld                    caco#
     C                   kfld                    kkzip
     C                   kfld                    kkefdt
     C*
     C     rvky1         klist
     C                   kfld                    kkcpt4
     C                   kfld                    kkmod
     C*** Beg Chg *** 10/01/10 **************************************
     C*****              kfld                    cafdat
     C                   kfld                    kkefdt
     C*** End Chg *** 10/01/10 **************************************
     C*
     C     rvky2         klist
     C                   kfld                    kkcpt4
     C                   kfld                    kkmod
     C*
     C     acky1         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C*** Beg Chg *** 10/01/10 **************************************
     C*****              kfld                    cafdat
     C                   kfld                    kkefdt
     C*** End Chg *** 10/01/10 **************************************
     C*
     C     acky2         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C*
     C     rcky1         klist
     C                   kfld                    caco#
     C                   kfld                    cazipa
     C                   kfld                    cacpt4
     C*** Beg Chg *** 10/01/10 **************************************
     C****               kfld                    cafdat
     C                   kfld                    kkefdt
     C*** End Chg *** 10/01/10 **************************************
     c*
     C     rcky2         klist
     C                   kfld                    caco#
     C                   kfld                    cazipa
     C                   kfld                    cacpt4
     C*
     C     amky1         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C                   kfld                    cacpt4
     C*** Beg Chg *** 10/01/10 **************************************
     C****               kfld                    cafdat
     C                   kfld                    kkefdt
     C*** End Chg *** 10/01/10 **************************************
     c*
     C     amky2         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C                   kfld                    cacpt4
     C*
     C     deky1         klist
     C                   kfld                    cacpt4
     C                   kfld                    xxmod1
     C                   kfld                    xxmod2
     C*** Beg Chg *** 10/01/10 **************************************
     C****               kfld                    cafdat
     C                   kfld                    kkefdt
     C*** End Chg *** 10/01/10 **************************************
     c*
     C     deky2         klist
     C                   kfld                    cacpt4
     C                   kfld                    xxmod1
     C                   kfld                    xxmod2
     C*
     C     dfky1         klist
     C                   kfld                    cacpt4
     C*** Beg Chg *** 10/01/10 **************************************
     C*****              kfld                    cafdat
     C                   kfld                    kkefdt
     C*** End Chg *** 10/01/10 **************************************
     c*
     C     dfky2         klist
     C                   kfld                    cacpt4
     c*
     C     gpky1         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C*** Beg Chg *** 10/01/10 **************************************
     C****               kfld                    cafdat
     C                   kfld                    kkefdt
     C*** End Chg *** 10/01/10 **************************************
     C*
     C     gpky2         klist
     C                   kfld                    kkst
     C                   kfld                    kkloc
     C*
     C     umky1         klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C*** Beg Chg *** 10/01/10 **************************************
     C****               kfld                    cafdat
     C                   kfld                    kkefdt
     C*** End Chg *** 10/01/10 **************************************
     C*
     C     umky2         klist
     C                   kfld                    caco#
     C                   kfld                    cacpt4
     C*
     C*** Beg Add *** 10/01/10 **************************************
     C     rtky1         klist
     C                   kfld                    qqst
     C                   kfld                    xxtype
     C                   kfld                    cafdat
     C*
     C     rtky2         klist
     C                   kfld                    qqst
     C                   kfld                    xxtype
     C*
     C*** End Add *** 10/01/10 **************************************
     C*** Main Processing
     C*
     C*Set the pointer for the FDCAP from the RRN
     C*
     C                   move      ' '           @@rtcd
     C     @@rrn         chain(n)  pca01                              99
     C                   if        *in99 = *on
     C                   Move      'F'           @@rtcd
     C                   return
     C*
     C                   else                                                   *in99=*on
     C*
     C     eoky          chain     peo01
     C*
     C                   if        %found
     C* open up FMZRPxx to calculate the locality
     C                   movel(p)  qca(1)        qqcmd
     C                   movel     eoast         qqst
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm      56            length           15 5
     C*
     C                   open      fmzrl
     C*
     C                   eval      kkloc = 99
     C*
     C*** Beg Add *** 10/01/10 *************************************************
     C                   z-add     cafdat        kkefdt
     C*
     C                   movel     'M'           xxtype
     C     rtky1         setgt     prt01
     C     rtky2         readpe    prt01                                  99
     C*
     C                   dow       *in99 = *off
     C                   if        rtstat = 'I'
     C     rtky2         readpe    prt01                                  99
     C                   else
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off and
     C                             rtmrel = '1'
     C                   z-add     rtefdt        kkefdt
     C                   else
     C*** End Add *** 10/01/10 *************************************************
     C                   z-add     cafdat        kkefdt
     C*** Beg Add *** 10/01/10 *************************************************
     C                   endif                                                  rtmrel = '1'
     C*
     C*** End Add *** 10/01/10 *************************************************
     C                   movel     eoflic        kkzip
     C*
     C     zrky          setgt     pzr01                              99
     C                   readp     pzr01                                  99
     C                   dow       *in99 = *off
     C                   z-add     zrefdt        k1efdt
     C                   if        kkzip >= zrfzcd and kkzip <= zrtzcd
     C                             and zrefdt = k1efdt
     C                   move      zrrgn#        kkloc
     C                   leave
     C                   endif
     C                   readp     pzr01                                  99
     C                   enddo
       //*** Beg Add *** 08/06/25 ******************************************
     C* Convert kkLoc for PA since the state file has 4 localities and
     C* Medicare has only two
       If EOast = 'PA';
          If kkLoc = 2 or kkLoc = 3 or kkLoc = 4;
             kkLoc = 99;
          Endif;
       Endif;
       //*** End Add *** 08/06/25 ******************************************
     C*
     C                   select
     C                   when      eobtyp = 'N'
     C     dhky          chain     pdh01
     C                   when      eobtyp = 'H'
     C     dhky          chain     pdu01
     C                   endsl
     C*** Initialising
     C                   eval      @@alw1 = 0
     C                   eval      @@alw2 = 0
     C                   eval      @@alw3 = 0
     C                   eval      xxamt1 = 0
     C                   eval      xxamt2 = 0
     C                   eval      mod = *blanks
     C* Anes
     C                   select
     C*
     C                   when      cacpt4 >= '00100' and cacpt4 <= '01999' and
     C                             %subst(cacpt4:5:1) <> 'F' and
     C                             %subst(cacpt4:5:1) <> 'T'
     C                             and (@@call = 'A'
     C                             or casrc = 'R')
     C*** Beg Add *** 10/01/10 ****************************************
     C                             or cacpt4 = '99100' and casrc = 'R'
     C                             or cacpt4 = '99116' and casrc = 'R'
     C                             or cacpt4 = '99135' and casrc = 'R'
     C                             or cacpt4 = '99140' and casrc = 'R'
     C*** End Add *** 10/01/10 ****************************************
     C                   exsr      anes
     C                   eval      @@eob = '395'
     C                   if        @@alw1 = 0
     C                   exsr      recu
     C                   endif
     C* Clinical Lab
     C                   when      casrc = 'L'
     C                   exsr      clab
     C                   eval      @@eob = '395'
     C                   if        @@alw1 = 0
     C                   exsr      recu
     C                   endif
     C*
     C                   when      casrc = 'F'
     C                   exsr      drgf
     C                   eval      @@eob = '395'
     C                   if        @@alw1 = 0
     C                   exsr      recu
     C                   endif
     C*
     C                   when      casrc = 'A'
     C                   exsr      amb
     C                   eval      @@eob = '395'
     C                   if        @@alw1 = 0
     C                   exsr      recu
     C                   endif
     C*
     C                   when      casrc = 'D'
     C                   exsr      dme
     C                   eval      @@eob = '395'
     C                   if        @@alw1 = 0
     C                   exsr      recu
     C                   endif
     C*
     C                   when      casrc = 'R' and
     C                             ((cacpt4 >= 'A0001' and cacpt4 <= 'Z9999')
     C                             or (cacpt4 >= '10000' and cacpt4 <= '99999')
     C                             or %subst(cacpt4:5:1) =  'F'  or
     C                              %subst(cacpt4:5:1) = 'T')
     C                   exsr      phy
     C                   if        @@alw1 = 0
     C                   exsr      recu
     C                   endif
     C*
     C                   endsl                                                  CASRC
     C*
     C                   endif                                                  FAEOL
     C* Closing the FMZRLxx
     C                   movel(p)  qca(2)        qqcmd
     C*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm      56            length           15 5
     C*
     C                   close     fmzrl
     C*
     C                   endif                                                  FDCAP
     C*
     C                   return
    C*****************************************************************
    C*  Anes - Calculating Amesthesia amount
    C*****************************************************************
     C     Anes          begsr
     C*
     C                   select
     C                   when      eobtyp = 'N'
     C                   movel     dhmod1        mod(1)
     C                   movel     dhmod2        mod(2)
     C                   movel     dhmod3        mod(3)
     C                   movel     dhmod4        mod(4)
     C                   when      eobtyp = 'H'
     C                   movel     dupmod        mod(1)
     C                   movel     dumod2        mod(2)
     C                   movel     dumod3        mod(3)
     C                   movel     dumod4        mod(4)
     C                   endsl
     C*
     C                   move      *blanks       kkmod
     C                   select
     C                   when      mod(1) <> ' '
     C                   movel     mod(1)        kkmod
     C                   when      mod(2) <> ' '
     C                   movel     mod(2)        kkmod
     C                   when      mod(3) <> ' '
     C                   movel     mod(3)        kkmod
     C                   when      mod(4) <> ' '
     C                   movel     mod(4)        kkmod
     C                   endsl
     C                   movel     cacpt4        kkcpt4
     C                   movel     eoast         kkst
     C*** Beg Add *** 06/22/11 *********************************************
     C                   if        kkst = 'P1'
     C                   movel     'PA'          kkst
     C                   endif
     C*** End Add *** 06/22/11 *********************************************
     C*** Beg Add *** 12/14/11 *********************************************
     C                   if        kkst = 'P2'
     C                   movel     'PA'          kkst
     C                   endif
     C*** End Add *** 12/14/11 *********************************************
     C                   if        kkmod = 'QX' or kkmod = 'QY' or
     C                             kkmod = 'QK' or kkmod = 'AA' or
     C                             kkmod = 'P1' or kkmod = 'P2' or
     C                             kkmod = 'P3' or kkmod = 'P4' or
     C                             kkmod = 'P5' or
     C                             kkmod = 'QZ' or kkmod = 'AD'
     C                   eval      kkmod = '  '
     C                   endif
     C*
     C*** read from ftrvl1
     C     rvky1         setgt     prv01
     C     rvky2         readpe    prv01                                  99
     C                   dow       *in99 = *off
     C                   if        rvstat = 'I'  and *in99 = *off
     C     rvky2         readpe    prv01                                  99
     C                   elseif    rvstat = 'A'
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
     C*** read from fmacl1 to find out the conversion factor
     C     acky1         setgt     pac01
     C     acky2         readpe    pac01                                  99
     C                   dow       *in99 = *off
     C                   if        acstat = 'I'  and *in99 = *off
     C     acky2         readpe    pac01                                  99
     C                   elseif    acstat = 'A'
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
     C*
     C                   select
     C*** Rounding criteria
     C                   when      @@rnd = 'M'
     C                   select
     C                   when      eobtyp = 'N'
     C                   eval(h)   xxunts = dhunts/15
     C                   when      eobtyp = 'H'
     C                   eval(h)   xxunts = duunts/15
     C                   endsl
     C*
     C                   endsl
     C*** Anesthesia amount = (base units + no. of service units/15) * conversion factor
     C                   if        rvwkrv <> 0
     C                   eval      xxamt1 = (rvwkrv + xxunts) * accf
     C                   endif
     C*
     C*** If rvwkrv is zero, we will get base units from FMRCP.
     C*** For reading from FMRCP, we have to get GEO zips.
     C
     C                   select
     C*** Modifier Reductions
     C                   when      @@modr = 'Y'
     C                   exsr      calmodr
     C*
     C                   endsl
     C*
     C                   if        @@alw1 = 0
     C                   eval      @@alw1 = xxamt1
     C                   endif                                                  @@alw1 = 0
     C*
     C                   if        @@call = 'F'
     C*** Beg del *** 10/01/10 ********************************************
     C*****              if        cacpt4 = '99100' or
     C*****                        cacpt4 = '99116' or
     C*****                        cacpt4 = '99135' or
     C*****                        cacpt4 = '99140'
     C*****              eval      @@alw1 = 0
     C*****              else
     C*** End Add *** 10/01/10 ********************************************
     C                   eval      @@alw1 = accf
     C*** Beg del *** 10/01/10 ********************************************
     C*****              endif
     C*** End del *** 10/01/10 ********************************************
     C                   eval      @@alw2 = rvwkrv
     C                   endif
     C*
     C                   endif                                                  FMACL1
     C                   endif                                                  FTRVL1
     C*
     C     endanes       endsr
    C*****************************************************************
    C*  Phy  - Calculating Physician amount
    C*****************************************************************
     C     Phy           begsr
     C*
     C                   eval      @@eob = '395'
     C*
     C                   select
     C                   when      eobtyp = 'N'
     C                   movel     dhmod1        mod(1)
     C                   movel     dhmod2        mod(2)
     C                   movel     dhmod3        mod(3)
     C                   movel     dhmod4        mod(4)
     C                   when      eobtyp = 'H'
     C                   movel     dupmod        mod(1)
     C                   movel     dumod2        mod(2)
     C                   movel     dumod3        mod(3)
     C                   movel     dumod4        mod(4)
     C                   endsl
     C*
     C                   move      *blanks       kkmod
     C                   movel     cacpt4        kkcpt4
     C                   movel     eoast         kkst
     C*** Beg Add *** 06/22/11 *********************************************
     C                   if        kkst = 'P1'
     C                   movel     'PA'          kkst
     C                   endif
     C*** End Add *** 06/22/11 *********************************************
     C*** Beg Add *** 12/14/11 *********************************************
     C                   if        kkst = 'P2'
     C                   movel     'PA'          kkst
     C                   endif
     C*** End Add *** 12/14/11 *********************************************
     C***  to find gpci's involed in final calculations
     C     gpky1         setgt     pgp01
     C     gpky2         readpe    pgp01                                  99
     C                   dow       *in99 = *off
     C                   if        gpstat = 'I'  and *in99 = *off
     C     gpky2         readpe    pgp01                                  99
     C                   elseif    gpstat = 'A'
     C                   leave
     C                   endif
     C                   enddo
     C                   if        *in99 = *off
     C*** read from ftrvl1
     C*** look up into modifier
     C     '26'          lookup    mod                                    99
     C                   if        *in99 = *on
     C                   eval      kkmod = '26'
     C                   else
     C     '53'          lookup    mod                                    99
     C                   if        *in99 = *on
     C                   eval      kkmod = '53'
     C                   else
     C     'TC'          lookup    mod                                    99
     C                   if        *in99 = *on
     C                   eval      kkmod = 'TC'
     C                   endif
     C                   endif
     C                   endif
     C*
     C*** Beg Del *** 05/14/13 *********************************************
     C**************
     C**************     if        kkmod = ' '
     C**************     select
     C**************     when      mod(1) <> ' '
     C**************     movel     mod(1)        kkmod
     C**************     when      mod(2) <> ' '
     C**************     movel     mod(2)        kkmod
     C**************     when      mod(3) <> ' '
     C**************     movel     mod(3)        kkmod
     C**************     when      mod(4) <> ' '
     C**************     movel     mod(4)        kkmod
     C**************     endsl
     C**************     if        kkmod = '50' or kkmod = '51' or
     C**************               kkmod = '62' or kkmod = '80' or
     C**************               kkmod = '81' or kkmod = '82'
     C**************     eval      kkmod = '  '
     C**************     endif
     C**************
     C**************     endif                                                  kkmod = ' '
     C*** End Del *** 05/14/13 *********************************************
     C*
     C     rvky1         setgt     prv01
     C     rvky2         readpe    prv01                                  99
     C                   dow       *in99 = *off
     C                   if        rvstat = 'I'  and *in99 = *off
     C     rvky2         readpe    prv01                                  99
     C                   elseif    rvstat = 'A'
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
     C*
     C                   select
     C*
     C                   when      eobtyp = 'N'
     C*
     C                   select
     C                   when      dhpos =  ' 11'
     C*** Beg Add *** 01/24/13 ****************************************
     C                   if        cafdat < 01012013
     C*** End Add *** 01/24/13 ****************************************
     C                   eval      xxamt1 = ((rvwkrv * gpwkgp) +
     C                                      (rvtnpe * gppegp) +
     C                                      (rvmprv * gpmpgp)) * rvconv
     C*** Beg Add *** 01/24/13 ****************************************
     C                   else
     C                   eval      xxamt1 = ((rvwkrv * gpwkgp) +
     C                                       (rvfnpe * gppegp) +
     C                                       (rvmprv * gpmpgp)) * rvconv
     C                   endif
     C*** End Add *** 01/24/13 ****************************************
     C                   when      dhpos <> ' 11'
     C*** Beg Add *** 01/24/13 ****************************************
     C                   if        cafdat < 01012013
     C*** End Add *** 01/24/13 ****************************************
     C                   eval      xxamt1 = ((rvwkrv * gpwkgp) +
     C                                      (rvtfpe * gppegp) +
     C                                      (rvmprv * gpmpgp)) * rvconv
     C*** Beg Add *** 01/24/13 ****************************************
     C                   else
     C                   eval      xxamt1 = ((rvwkrv * gpwkgp) +
     C                                       (rvffpe * gppegp) +
     C                                       (rvmprv * gpmpgp)) * rvconv
     C                   endif
     C*** End Add *** 01/24/13 ****************************************
     C                   endsl
     C*
     C                   when      eobtyp = 'H'
     C*
     C                   select
     C                   when      dupos =  ' 11'
     C*** Beg Add *** 01/24/13 ****************************************
     C                   if        cafdat < 01012013
     C*** End Add *** 01/24/13 ****************************************
     C                   eval      xxamt1 = ((rvwkrv * gpwkgp) +
     C                                      (rvtnpe * gppegp) +
     C                                      (rvmprv * gpmpgp)) * rvconv
     C*** Beg Add *** 01/24/13 ****************************************
     C                   else
     C                   eval      xxamt1 = ((rvwkrv * gpwkgp) +
     C                                       (rvfnpe * gppegp) +
     C                                       (rvmprv * gpmpgp)) * rvconv
     C                   endif
     C*** End Add *** 01/24/13 ****************************************
     C                   when      dupos <> ' 11'
     C*** Beg Add *** 01/24/13 ****************************************
     C                   if        cafdat < 01012013
     C*** End Add *** 01/24/13 ****************************************
     C                   eval      xxamt1 = ((rvwkrv * gpwkgp) +
     C                                      (rvtfpe * gppegp) +
     C                                      (rvmprv * gpmpgp)) * rvconv
     C*** Beg Add *** 01/24/13 ****************************************
     C                   else
     C                   eval      xxamt1 = ((rvwkrv * gpwkgp) +
     C                                       (rvffpe * gppegp) +
     C                                       (rvmprv * gpmpgp)) * rvconv
     C                   endif
     C*** End Add *** 01/24/13 ****************************************
     C                   endsl
     C*
     C                   endsl                                                  eobty = 'N'/'H'
     C*
     C                   select
     C                   when      @@modr = 'Y'
     C                   exsr      calmodr
     C                   endsl
     C*** If rvwkrv is zero, we will get base units from FMRCP.
     C*** For reading from FMRCP, we have to get GEO zips.
     C*
     C                   if        @@alw1 = 0
     C                   eval      @@alw1 = xxamt1
     C                   endif                                                  @@alw1 = 0
     C*
     C                   endif                                                  *in99 = *off
     C*
     C                   endif                                                  *in99 = *off
     C*
     C     endphy        endsr
    C*****************************************************************
    C*  Clab - Calculating Clinical lab amount
    C*****************************************************************
     C     Clab          begsr
     C*
     C                   select
     C                   when      eobtyp = 'N'
     C                   movel     dhmod1        mod(1)
     C                   movel     dhmod2        mod(2)
     C                   movel     dhmod3        mod(3)
     C                   movel     dhmod4        mod(4)
     C                   when      eobtyp = 'H'
     C                   movel     dupmod        mod(1)
     C                   movel     dumod2        mod(2)
     C                   movel     dumod3        mod(3)
     C                   movel     dumod4        mod(4)
     C                   endsl
     C*
     C                   move      *blanks       kkmod
     C                   movel     cacpt4        kkcpt4
     C                   movel     eoast         kkst
     C*** Beg Add *** 06/22/11 *********************************************
     C                   if        kkst = 'P1'
     C                   movel     'PA'          kkst
     C                   endif
     C*** End Add *** 06/22/11 *********************************************
     C*** Beg Add *** 12/14/11 *********************************************
     C                   if        kkst = 'P2'
     C                   movel     'PA'          kkst
     C                   endif
     C*** End Add *** 12/14/11 *********************************************
     C*
     C                   select
     C                   when      mod(1) <> ' '
     C                   movel     mod(1)        kkmod
     C                   when      mod(2) <> ' '
     C                   movel     mod(2)        kkmod
     C                   when      mod(3) <> ' '
     C                   movel     mod(3)        kkmod
     C                   when      mod(4) <> ' '
     C                   movel     mod(4)        kkmod
     C                   endsl
       // Beg Add *** 06/21/18 *********************************************
      /Free
                         If EOast = 'TX';
                            kkMod = *blanks;
                         Endif;
      /End-Free
       // End Add *** 06/21/18 *********************************************
     C*** read from ftlbl1
     C     rvky1         setgt     plb01
     C     rvky2         readpe    plb01                                  99
     C                   dow       *in99 = *off
     C                   if        lbstat = 'I'  and *in99 = *off
     C     rvky2         readpe    plb01                                  99
     C                   elseif    lbstat = 'A'
     C                   leave
     C                   endif
     C                   enddo
       // Beg Add *** 06/21/18 *********************************************
      /Free
                         If *in99 = *on and EOast = 'TX';
                            kkMod = 'QW';
                            Setgt RVky1 Plb01;
                            Readpe RVky2 plb01;
                            If not %Eof(ftlbl1);
                               *in99 = *off;
                            Endif;
                         Endif;
      /End-Free
       // End Add *** 06/21/18 *********************************************
     C*
     C                   if        *in99 = *off
     C*
     C                   select
     C***
     C                   when      kkst = 'AR'
     C                   eval      xxamt1 = lbar
     C*
     C                   when      kkst = 'DC'
     C                   eval      xxamt1 = lbdc
     C*
     C                   when      kkst = 'TN'
     C                   eval      xxamt1 = lbtn
     C*
     C                   when      kkst = 'TX'
     C                   eval      xxamt1 = lbtx
     C*
     C                   when      kkst = 'NJ'
     C                   eval      xxamt1 = lbnj
     C*
     C*** Beg Add *** 10/01/10 *************************************************
     C                   when      kkst = 'WV'
     C                   eval      xxamt1 = lbwv
     C*
     C*** End Add *** 10/01/10 *************************************************
     C*
     C*** Beg Add *** 01/06/16 *************************************************
     C                   when      kkst = 'OK'
     C                   eval      xxamt1 = lbok
     C*
     C*** End Add *** 01/06/16 *************************************************
     C*
     C*** Beg Add *** 06/22/11 *********************************************
     C                   when      kkst = 'PA'
     C                   eval      xxamt1 = lbpa
     C*** End Add *** 06/22/11 *********************************************
     C                   endsl
       //*** Beg Add *** 01/09/18 ****************************************
      /Free
                         if kkEfDt >= 1180101;
                            eval xxAmt1 = LBnLim;
                         Endif;
      /End-free
       //*** End Add *** 01/09/18 ****************************************
     C*** Return EOB code as 395
     C*** If xxamt1 is zero, we will get amount from FMRCP.
     C*** For reading from FMRCP, we have to get GEO zips.
     C
     C                   select
     C*** Modifier Reductions
     C                   when      @@modr = 'Y'
     C                   exsr      calmodr
     C*
     C                   endsl
     C*
     C                   if        @@alw1 = 0
     C                   eval      @@alw1 = xxamt1
     C                   endif                                                  @@alw1 = 0
     C*
     C                   endif                                                  FTLBL1\
     C*
     C     endclab       endsr
    C*****************************************************************
    C*  Amb  - Calculating Ambulance amount
    C*****************************************************************
     C     Amb           begsr
     C*
     C                   movel     eoast         kkst
     C*** Beg Add *** 06/22/11 *********************************************
     C                   if        kkst = 'P1'
     C                   movel     'PA'          kkst
     C                   endif
     C*** End Add *** 06/22/11 *********************************************
     C*** Beg Add *** 12/14/11 *********************************************
     C                   if        kkst = 'P2'
     C                   movel     'PA'          kkst
     C                   endif
     C*** End Add *** 12/14/11 *********************************************
     C                   movel     cacpt4        kkcpt4
     c*** Beg Chg *** 04/12/11 **************************************
     c                   if        kkst = 'KY' and kkloc = 99
     c                   eval      kkloc = 0
     c                   endif
     c*** End Chg *** 04/12/11 **************************************
     C*** read from ftrvl1
     C     amky1         setgt     pam01
     C     amky2         readpe    pam01                                  99
     C                   dow       *in99 = *off
     C                   if        amstat = 'I'  and *in99 = *off
     C     amky2         readpe    pam01                                  99
     C                   elseif    amstat = 'A'
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
     C*
     C*** Beg Chg *** 09/02/11 **************************************
     C******             if        cacpt4 >= 'A0425' and cacpt4 <= 'A0429'
     C******                       or cacpt4 >= 'A0433' and cacpt4 <= 'A0434'
     C                   if        cacpt4 = 'A0425'
     C*** End Chg *** 09/02/11 **************************************
     C                   select
     C***
     C                   when      zrarea = 'U'
     C                   eval      xxamt1 = amurbn * caunts
     C*
     C                   when      caunts <= 17 and zrarea = 'R'
     C                             and caunts >= 1
     C                   eval      xxamt1 = amrgrd * caunts
     C*
     C                   when      caunts > 17 and zrarea = 'R'
     C                   eval      xxamt1=(amrurl*1.5*17) + (amrurl*(caunts-17))
     C*
     C*** Beg Add *** 09/02/11 **************************************
     C                   when      caunts > 17 and zrarea = 'S'                 Super Rural > 17 mil
     C                   if        amlqsr > 0
     C                   eval      xxrate = amlqsr
     C                   else
     C                   eval      xxrate = amrgrd
     C                   endif
     C                   eval      xxamt1=(amrgrd*17) + ((caunts-17)*xxrate)

     C                   when      caunts <= 17 and zrarea = 'S'                Super Rural <=17 mil
     C                   if        amlqsr > 0
     C                   eval      xxrate = amlqsr
     C                   else
     C                   eval      xxrate = amrgrd
     C                   endif
     C                   eval      xxamt1 = caunts * xxrate
     C*** End Add *** 09/02/11 **************************************
     C*
     C*** Beg Chg *** 04/12/11 **************************************
     C                   when      zrarea = ' '
     C                   eval      xxamt1 = amurbn
     C*** End Chg *** 04/12/11 **************************************
     C*
     C                   endsl
     C*
     C                   endif                                                  ground ambulance mil
     C*
     C*** Beg Chg *** 09/02/11 **************************************
     C*******            if        cacpt4 >='A0431' and cacpt4 <= 'A0432'
     C*******                      or cacpt4 >= 'A0435' and cacpt4 <= 'A0436'
     C                   if        cacpt4 >='A0430' and cacpt4 <= 'A0431'
     C                             or cacpt4 >= 'A0435' and cacpt4 <= 'A0436'
     C*** End Chg *** 09/02/11 **************************************
     C                   select
     C***
     C                   when      zrarea = 'U'
     C                   eval      xxamt1 = amurbn * caunts
     C*
     C                   when      zrarea = 'R'
     C                   eval      xxamt1 = amurbn * 1.5 *caunts
     C*** Beg Add *** 09/02/11 **************************************
     C                   when      zrarea = 'S'                                 Super Rural
     C                   if        amlqsr > 0
     C                   eval      xxrate = amlqsr
     C                   else
     C                   eval      xxrate = amurbn
     C                   endif
     C                   eval      xxamt1 = xxrate * 1.5 * caunts
     C*** End End *** 09/02/11 **************************************
     c*** Beg Chg *** 04/12/11 **************************************
     C                   when      zrarea = ' '
     C                   eval      xxamt1 = amurbn
     c*** End Chg *** 04/12/11 **************************************
     C*
     C                   endsl
     C*
     C                   endif                                                  air ambulance mil
     C*** Beg Add *** 09/02/11 **************************************
     C                   if        cacpt4 >='A0426' and cacpt4 <= 'A0429'
     C                   select
     C*
     C                   when      zrarea = 'U'
     C                   eval      xxamt1 = amurbn * caunts
     C*
     C                   when      zrarea = 'R'
     C                   eval      xxamt1 = amrurl * caunts
     C*
     C                   when      zrarea = 'S'
     C                   eval      xxamt1 = amlqsr * caunts
     C*
     C                   endsl
     C                   endif                                                  air ambulance mil
     C*
     C*
     C                   if        cacpt4 >='A0432' and cacpt4 <= 'A0434'
     C                   select
     C*
     C                   when      zrarea = 'U'
     C                   eval      xxamt1 = amurbn * caunts
     C*
     C                   when      zrarea = 'R'
     C                   eval      xxamt1 = amrurl * caunts
     C*
     C                   when      zrarea = 'S'
     C                   eval      xxamt1 = amlqsr * caunts
     C*
     C                   endsl
     C                   endif                                                  air ambulance mil
     C*** End Add *** 09/02/11 **************************************
     C*
     C*** Return EOB code as 395
     C                   select
     C*** Modifier Reductions
     C                   when      @@modr = 'Y'
     C                   exsr      calmodr
     C*
     C                   endsl
     C*
     C                   if        @@alw1 = 0
     C                   eval      @@alw1 = xxamt1
     C                   endif                                                  @@alw1 = 0
     C*
     C                   endif                                                  FMAML1
     C*
     C     endamb        endsr
    C*****************************************************************
    C*  Dme  - Calculating DMEPOS amount
    C*****************************************************************
     C     Dme           begsr
     C*
     C                   select
     C                   when      eobtyp = 'N'
     C                   movel     dhmod1        mod(1)
     C                   movel     dhmod2        mod(2)
     C                   movel     dhmod3        mod(3)
     C                   movel     dhmod4        mod(4)
     C                   when      eobtyp = 'H'
     C                   movel     dupmod        mod(1)
     C                   movel     dumod2        mod(2)
     C                   movel     dumod3        mod(3)
     C                   movel     dumod4        mod(4)
     C                   endsl
     C*
     C                   movel     eoast         kkst
     C*** Beg Add *** 06/22/11 *********************************************
     C                   if        kkst = 'P1'
     C                   movel     'PA'          kkst
     C                   endif
     C*** End Add *** 06/22/11 *********************************************
     C*** Beg Add *** 12/14/11 *********************************************
     C                   if        kkst = 'P2'
     C                   movel     'PA'          kkst
     C                   endif
     C*** End Add *** 12/14/11 *********************************************
     C                   movel     cacpt4        kkcpt4
     C                   eval      xxdme = ' '
     C*** read from fmdel1
     C                   eval      i = 0
     C*
     C                   dou       *in99 = *off
     C                   eval      i = i + 1
     C*
     C                   if        i  <  5
     C                   dow       mod(i) = ' '
     C                   eval      i = i + 1
     C                   if        i > 4
     C                   leave
     C                   endif
     C                   enddo                                                  mod(i) = ' '
     C                   endif                                                  i < 5
     C                   if        i >=5
     C                   leave
     C                   endif                                                  i >= 5
     C*
     C                   eval      j = 0
     C*
     C                   dow       j <> 5 and j + 1 <> 5
     C                   eval      j = j + 1
     C*
     C                   dow       mod(j) = ' '
     C                   eval      j = j + 1
     C                   if        j > 4
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        j <> i
     C*
     C                   if        i <> 5  and j <> 5
     C*
     C                   if        mod(i) <> ' ' and mod(j) <>  ' '
     C                   eval      xxmod1 = mod(i)
     C                   eval      xxmod2 = mod(j)
     C*
     C     deky1         setgt     pde01                              99
     C     deky2         readpe    pde01                                  99
     C                   if        *in99 = *off
     C                   select
     C                   when      kkst = 'NJ'
     C                   eval      xxamt1 = denj
     C                   when      kkst = 'WV'
     C                   eval      xxamt1 = dewv
     C*** Beg Add *** 06/22/11 *********************************************
     C                   when      kkst = 'PA'
     C                   eval      xxamt1 = depa
     C*** End Add *** 06/22/11 *********************************************
     C*** Beg Add *** 08/16/11 *********************************************
     C                   when      kkst = 'CT'
     C                   eval      xxamt1 = dect
     C*** End Add *** 08/16/11 *********************************************
     C*** Beg Add *** 04/30/13 *********************************************
     C                   when      kkst = 'FL'
     C                   eval      xxamt1 = defl
     C*** End Add *** 04/30/13 *********************************************
     C*** Beg Add *** 04/04/16 *********************************************
     C                   when      kkst = 'OK'
     C                   eval      xxamt1 = deok
     C*** End Add *** 04/04/16 *********************************************
     C*** Beg Add *** 06/21/18 *********************************************
     C                   when      kkst = 'TX'
     C                   eval      xxamt1 = DEtx
     C*** End Add *** 06/21/18 *********************************************
     C                   endsl
     C*
     C                   eval      xxmod2 = ' '
     C                   eval      xxdme = 'Y'
     C                   leave
     C                   endif                                                  if *in99 = *off
     C                   endif                                                  mod(i) & mod(j)<>' '
     C*
     C                   endif                                                  i<>5 & j<>5
     C*
     C                   endif                                                  j <> i
     C*
     C                   enddo                                                  j <> 5
     C*
     C                   enddo                                                  dou *in99 = *off
     C*
     C                   if        xxdme <> 'Y'
     C*
     C                   eval      xxmod1 = ' '
     C                   select
     C                   when      mod(1) <> ' ' and mod(2) = ' ' and mod(3) =
     C                             ' ' and mod(4) = ' '
     C                   eval      xxmod1 = mod(1)
     C                   when      mod(1) = ' ' and mod(2) <> ' ' and mod(3) =
     C                             ' ' and mod(4) = ' '
     C                   eval      xxmod1 = mod(2)
     C                   when      mod(1) = ' ' and mod(2) = ' ' and mod(3) <>
     C                             ' ' and mod(4) = ' '
     C                   eval      xxmod1 = mod(3)
     C                   when      mod(1) = ' ' and mod(2) = ' ' and mod(3) =
     C                             ' ' and mod(4) <> ' '
     C                   eval      xxmod1 = mod(4)
     C                   endsl
     C                   eval      xxmod2 = ' '
     C*** Beg Add *** 08/06/25 ******************************************
       // PA requires a modifier for DME and sometimes Medicare does not
       // always require NU so we will try NU then blank it out and try
       If EOast = 'PA' and xxMod1 = 'NU';
          Setgt deky1 pde01;
          Readpe deky2 pde01;
          If %EOF (FMDEL1);
             xxMod1 = *blanks;
          Endif;
       Endif;
     C*** End Add *** 08/06/25 ******************************************
     C     deky1         setgt     pde01                              99
     C     deky2         readpe    pde01                                  99

     C                   if        *in99 = *off
     C                   select
     C                   when      kkst = 'NJ'
     C                   eval      xxamt1 = denj
     C*** Beg Add *** 10/01/10 *************************************************
     C                   when      kkst = 'WV'
     C                   eval      xxamt1 = dewv
     C*** End Add *** 10/01/10 *************************************************
     C*** Beg Add *** 06/22/11 *********************************************
     C                   when      kkst = 'PA'
     C                   eval      xxamt1 = depa
     C*** End Add *** 06/22/11 *********************************************
     C*** Beg Add *** 08/16/11 *********************************************
     C                   when      kkst = 'CT'
     C                   eval      xxamt1 = dect
     C*** End Add *** 08/16/11 *********************************************
     C*** Beg Add *** 04/30/13 *********************************************
     C                   when      kkst = 'FL'
     C                   eval      xxamt1 = defl
     C*** End Add *** 04/30/13 *********************************************
     C*** Beg Add *** 04/04/16 *********************************************
     C                   when      kkst = 'OK'
     C                   eval      xxamt1 = deok
     C*** End Add *** 04/04/16 *********************************************
     C*** Beg Add *** 06/21/18 *********************************************
     C                   when      kkst = 'TX'
     C                   eval      xxamt1 = DEtx
     C*** End Add *** 06/21/18 *********************************************
     C                   endsl
     C*
     C                   eval      xxdme = 'Y'
     C                   endif                                                  if *in99 = *off
     C                   endif                                                  if xxdme <> 'Y'
     C*** Return EOB code as 395
     C*
     C                   eval      @@alw1 = xxamt1
     C*
     C     enddme        endsr
    C*****************************************************************
    C*  Drgf - Calculating Drug fee amount
    C*****************************************************************
     C     Drgf          begsr
     C*
     C                   movel     cacpt4        kkcpt4
     C*** read from ftrvl1
     C     dfky1         setgt     pdf01
     C     dfky2         readpe    pdf01                                  99
     C                   dow       *in99 = *off
     C                   if        dfstat = 'I'  and *in99 = *off
     C     dfky2         readpe    pdf01                                  99
     C                   elseif    dfstat = 'A'
     C                   leave
     C                   endif
     C                   enddo
     C*
     C                   if        *in99 = *off
     C*
     C                   eval      xxamt1 = dfrate
     C*
     C*** Return EOB code as 395
     C*** If xxamt1 is zero, we will get amount from FMRCP.
     C*** For reading from FMRCP, we have to get GEO zips.
     C
     C                   select
     C*** Modifier Reductions
     C                   when      @@modr = 'Y'
     C                   exsr      calmodr
     C*
     C                   endsl
     C*
     C                   if        @@alw1 = 0
     C                   eval      @@alw1 = xxamt1
     C                   endif                                                  @@alw1 = 0
     C*
     C                   endif                                                  FTLBL1\
     C*
     C     enddrgf       endsr
     C*****************************************************************
     C*** CALMODR - Calculating modifier reductions
     C*****************************************************************
     C     calmodr       begsr
     C*
     C     'QX'          lookup    mod                                    31
     C                   if        *in31 = *on
     C                   eval      @@alw1 = xxamt1 *.5
     C                   endif
     C     'QY'          lookup    mod                                    32
     C                   if        *in32 = *on
     C                   eval      @@alw1 = xxamt1 *.5
     C                   endif
     C     'QK'          lookup    mod                                    33
     C                   if        *in33 = *on
     C                   eval      @@alw1 = xxamt1 *.5
     C                   endif
     C     'P3'          lookup    mod                                    98
     C                   if        *in98 = *on
     C                   if        *in31 = '1' or *in32 = '1'
     C                              or  *in33 = '1'
     C                   eval      @@alw1 = @@alw1 + accf
     C                   else
     C                   eval      @@alw1 = xxamt1 + accf
     C                   endif
     C                   endif
     C     'P4'          lookup    mod                                    98
     C                   if        *in98 = *on
     C                   if        *in31 = '1' or *in32 = '1'
     C                              or  *in33 = '1'
     C                   eval      @@alw1 = @@alw1 +  (2 * accf)
     C                   else
     C                   eval      @@alw1 = xxamt1 + (2 * accf)
     C                   endif
     C                   endif
     C     'P5'          lookup    mod                                    98
     C                   if        *in98 = *on
     C                   if        *in31 = '1' or *in32 = '1'
     C                              or  *in33 = '1'
     C                   eval      @@alw1 = @@alw1 + (3 * accf)
     C                   else
     C                   eval      @@alw1 = xxamt1 + (3 * accf)
     C                   endif
     C                   endif
     C     '50'          lookup    mod                                    98
     C                   if        *in98 = *on
     C                   if        rvbila = '0' or
     C                             rvbila = '2' or
     C                             rvbila = '3' or
     C                             rvbila = '9'
     C*** return eob code = '388'
     C                   eval      @@eob = '388'
     C                   elseif    rvbila = '1'
     C*** return eob code = '389'
     C                   eval      xxamt1 = 1.5 * xxamt1
     C                   eval      @@eob = '389'
     C                   endif
     C                   endif
     C     '51'          lookup    mod                                    98
     C                   if        *in98 = *on
     C                   if        rvmult = '1' or
     C                             rvmult = '2'
     C                   eval      xxamt1 = xxamt1 * .5
     C*** return eob code = '389'
     C                   eval      @@eob = '389'
     C                   endif
     C                   if        rvmult = '0' or
     C                             rvmult = '3' or
     C                             rvmult = '4' or
     C                             rvmult = '9'
     C*** return eob code = '388'
     C                   eval      @@eob = '388'
     C                   endif
     C                   endif                                                  mod = '51'
     C     '62'          lookup    mod                                    98
     C                   if        *in98 = *on
     C                   if        rvcosg = '1' or
     C                             rvcosg = '2'
     C*** return eob code = '389'
     C                   eval      xxamt1 = xxamt1 * .625
     C                   eval      @@eob = '389'
     C                   endif
     C                   if        rvcosg = '0' or
     C                             rvcosg = '9'
     C*** return eob code = '388'
     C                   eval      @@eob = '388'
     C                   endif
     C                   endif                                                  mod = '51'
     C     '80'          lookup    mod                                    98
     C                   if        *in98 = *on
     C                   if        rvasst = '2'
     C                   eval      xxamt1 = xxamt1 * .16
     C*** return eob code = '389'
     C                   eval      @@eob = '389'
     C                   endif
     C                   if        rvasst = '0' or
     C                             rvasst = '1' or
     C                             rvasst = '9'
     C*** return eob code = '388'
     C                   eval      @@eob = '388'
     C                   endif
     C                   endif                                                  mod = '51'
     C     '81'          lookup    mod                                    98
     C                   if        *in98 = *on
     C                   if        rvasst = '2'
     C                   eval      xxamt1 = xxamt1 * .136
     C*** return eob code = '389'
     C                   eval      @@eob = '389'
     C                   endif
     C                   if        rvasst = '0' or
     C                             rvasst = '1' or
     C                             rvasst = '9'
     C*** return eob code = '388'
     C                   eval      @@eob = '388'
     C                   endif
     C                   endif                                                  mod = '51'
     C     '82'          lookup    mod                                    98
     C                   if        *in98 = *on
     C                   if        rvasst = '2'
     C                   eval      xxamt1 = xxamt1 * .16
     C*** return eob code = '389'
     C                   eval      @@eob = '389'
     C                   endif
     C                   if        rvasst = '0' or
     C                             rvasst = '1' or
     C                             rvasst = '9'
     C*** return eob code = '388'
     C                   eval      @@eob = '388'
     C                   endif
     C                   endif                                                  mod = '51'
     C*
     C                   endsr
     C*************************************************************
    C*************************************************************
     C     recu          begsr
    C*
    C*  Default to use service date for effective date for rules
     C*** Beg Del *** 10/01/10 ************************************
     C*****              eval      kkefdt = cafdat
     C*** End Del *** 10/01/10 ************************************
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
     C                   eval      @@alw1 = rcmra
     C                   eval      xxprof = rcprof
     C                   eval      xxtech = rc92n1
     c*
     C                   eval      @@rtcd = 'U'
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
     C                   endif                                                  end *in99
     C*
     C                   if        cacpt4 > '00100' and cacpt4 <= '01999'
     C                   eval      @@alw1 = (rcanam + xxunts) * rcconf
     C                   endif
     C*
     C                   endif                                                  end *in99
    C*
     C     recue         endsr
    C*************************************************************
**  TABLE QCA
OVRDBF FILE(FMZRL)   TOFILE(*LIBL/FMZRL  )
DLTOVR FILE(FMZRL)
