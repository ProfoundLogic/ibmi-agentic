     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H*  CREATED BY                                                   *
     H*                                                               *
     H*  10/17/11  C:251 Project: 114MAR                              *
     H*                                                               *
     H*  This program will calculate the IRF allowance and will       *
     H*  return 'F' if the records in files is not found and  will    *
     H*  return a '*' if the data is insufficiant to calculate the    *
     H*  the allowance                                                *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *                    *
     H*  11/13/14  C:212 Proj: FS448    Lip adjustment calculation :  *                    *
     H*                                 remove hardcoded value        *                    *
     H*                                                               *
     H*****************************************************************
     Ffmirl1    if   e           k disk
     Ffmhpl1    if   e           k disk
     Ffmmgl1    if   e           k disk
     Ffmiwl2    if   e           k disk
     Ffdcap     if   e             disk
     Ffddul     uf   e           k disk
     Ffaeul     if   e           k disk
     Ffaeol4    uf   e           k disk
     Fftrtl2    if   e           k disk
     Fftabl     if   e           k disk    usropn
     D*
     D                 ds
     D qqcmd                         56
     D  qqst                          2    overlay(qqcmd:38)
     D*
     D qca             s             56    dim(2) ctdata perrcd(1)
     D*
     D psds          esds                  extname(szq1p)
     D*
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D*
     D xxlbp_pmt       s              9  2
     D xxtch_adj       s             13  6
     D lip_adj         s             11  4
     D T_H_pmnt        s              9  2
     D NA_fed          s             11  4
     D NA_F_lbp        s             11  4
     D W_A_amt         s             11  4
     D NA_F_nlbp       s             11  4
     D W_A_fed         s             11  4
     D rur_adj         s             11  4
     D W_R_A_fed       s              9  2
     D W_R_L_A_fed     s             11  4
     D L_A_amt         s              9  2
     D L_A_pmt         s             11  4
     D T_S_A_amt       s              9  2
     D adj_pros_pmt    s             11  4
     D outl_lbp        s             11  4
     D W_A_outl        s             11  4
     D outl_nlbp       s             11  4
     D W_A_O_thrh      s             11  4
     D W_R_A_O_thrh    s             11  4
     D W_R_L_A_O_thrh  s             11  4
     D T_A_amt         s             11  4
     D H_O_amt         s             11  4
     D T_H_outl        s             11  4
     D tot_adj         s             11  4
     D O_ccr           s             11  4
     D H_O_pmnt        s              9  2
     D xxfed           s             11  4
     D CMG_brt         s             11  4
     D CMG_rlw         s              6  4
     D per_diem_1      s             11  4
     D pay_diem_1      s             11  4
     D per_diem_rem    s             11  4
     D pay_diem_rem    s             11  4
     Dxxcbs            s              2
     D*
     D xxlen           s              5  0
     D xxchar          s              1
     D kkcar           s                   like(irprov)
     D @@irfb          s                   like(cairfb)
     D @@outl          s                   like(caoutl)
     D @@lip           s                   like(calip)
     D @@tch           s                   like(catch)
     C*
     D xxunts          s                   like(duunts)
     D xxrelw          s                   like(mgrelw)
     D xxalos          s                   like(mgalos)
     D kkcode          s                   like(mgcode)
     D kktier          s                   like(mgtier)
     D xxcode          s                   like(mgcode)
     D @@code          s                   like(mgcode)
     D xxruradj        s                   like(rtirad)
     D*
     C     *entry        plist
     C                   parm                    @@rrn             9 0           RRN of FDCAP
     C                   parm                    @@call            1
     C                   parm                    @@alw1            9 2
     C                   parm                    @@alw2            9 2
     C                   parm                    @@alw3            9 2
     C                   parm                    @@outl
     C                   parm                    @@irfb
     C                   parm                    @@tch
     C                   parm                    @@lip
     C                   parm                    @@code
     C                   parm                    @@rtcd            1
     C*
     C     dtcymd1       plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     irky          klist
     C                   kfld                    kkcar
     C                   kfld                    kkffdt
     C*
     C     irky1         klist
     C                   kfld                    kkcar
     C*
     C     mgky          klist
     C                   kfld                    kkcode
     C                   kfld                    kktier
     C                   kfld                    kkefdt
     C*
     C     mgky1         klist
     C                   kfld                    kkcode
     C                   kfld                    kktier
     C*
     C     duky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    cadseq
     C*
     C     caky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C     caky#         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    cadseq
     C*
     C     rtky          klist
     C                   kfld                    kkst              2
     C                   kfld                    kktype            1
     C                   kfld                    kkefdt            7 0
     C*
     C     rtky1         klist
     C                   kfld                    kkst
     C                   kfld                    kktype
     C*
     C     iwky          klist
     C                   kfld                    kkcbsa
     C                   kfld                    kkefdt
     C*
     C     iwky1         klist
     C                   kfld                    kkcbsa
     C*
     C     *like         define    eoco#         kkco#
     C     *like         define    eosub#        kksub#
     C     *like         define    eofnd         kkfnd
     C     *like         define    eofyr         kkfyr
     C     *like         define    eocase        kkcase
     C     *like         define    eoeob#        kkeob#
     C     *like         define    irffdt        kkffdt
     C     *like         define    iracbs        kkcbsa
     C*
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *blanks       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*
     C*Set the pointer for the FDCAP from the RRN
     C*
     C     @@rrn         chain(n)  pca01                              99
     C                   if        *in99 = *on
     C                   move      'F'           @@rtcd
     C                   return
     C                   else
     C*
     C                   Z-add     caco#         kkco#
     C                   Z-add     casub#        kksub#
     C                   Z-add     cafnd         kkfnd
     C                   Z-add     cacase        kkcase
     C                   Z-add     caeob#        kkeob#
     C                   Z-add     cafyr         kkfyr
     C                   exsr      opnOverride
     C*
     C*Set the pointer for the FAEUL from the values of FDCAP
     C*
     C     caky          chain     peu01                              99
     C     caky          chain     peo01
     C                   endif
     C**************************************************************************
     C* MAIN CODE STARTS HERE
     C*
     C                   exsr      SrInit
     C*** Basic calc (steps 1 - 14)
     C                   exsr      SrBasic
     C*
     C                   select
     C*
     C                   when      duunts < xxalos and (eupats = '02' or
     C                             eupats = '03' or eupats = '61' or
     C                             eupats = '62' or eupats = '63' or
     C                             eupats = '64' )
     C*
     C** For transfer payments - Length of stay must be < average LOS for CMG, and
     C** Patient status must be (02,03,61,62,63, or 64)
     C*
     C                   exsr      SrTransP
     C*
     C                   when      (eupats <> '20' and eupats <> '40' and
     C                             eupats <> '41' and eupats <> '42') and
     C                             (durate <> 'A0901' and durate <> 'A0902'
     C                             and durate <> 'A0903' and durate <> 'A0904')
     C                             and  duunts > 3
     C*
     C** For regular payments - Patient should not expired/no orthopaedic codes and length of
     C** stay > 3 days
     C*
     C                   exsr      SrRegP
     C*
     C                   other
     C                   exsr      SrShortP
     C*
     C                   endsl
     C*
     C** Move back final payment to F4000N
     C*
     C                   update    peo01
     C*
     C                   eval      @@alw1 =  T_H_pmnt
     C*
     C                   eval      *inlr = *on
     C                   return
     C*
    C*================================================================
    C*  SrBasic  - Basic calculation stesp ( step 1-14)
    C*================================================================
     C     SrBasic       Begsr
     C*
     C* (2),(4), (9),(10),(11) Get data from FMIRP
     C*
     C                   move      cafdat        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         kkffdt
     C*
     C                   eval      kkcar = eumcar
     C*
     C     irky          setgt     pir01
     C     irky1         readpe    pir01
     C*
     C                   if        not %eof
     C                   eval      xxlen = %len(%trim(iracbs))
     C
     C                   if        iracbs  = '00000' or iracbs = *blanks
     C                   eval      xxlen = 0
     C                   endif
     C                   endif
     C*
     C                   eval      eucbsa = iracbs
     C                   move      iracbs        kkcbsa
     C                   Z-add     cafdat        kkefdt
     C*
     C* (3) Get the field IWWIND
     C*
     C     iwky          setgt     Piw01
     C     iwky1         readpe    Piw01
     C                   if        not %eof
     C                   eval      eowind = iwwind
     C                   else
     C                   eval      eowind = *zeros
     C                   endif
     C*
     C* (4) Get cpt4 code from FDDUP
     C*
     C     duky          chain     pdu01                              98
     C                   if        %found
     C*
     C* Get Tier (first char of CMG code from file FDDUP)
     C*
     C                   eval      xxchar = *blanks
     C                   eval      xxchar = %subst(durate:1:1)
     C                   select
     C                   when      xxchar = 'A'
     C                   eval      kktier = '0'
     C*
     C                   when      xxchar = 'B'
     C                   eval      kktier = '1'
     C*
     C                   when      xxchar = 'C'
     C                   eval      kktier = '2'
     C*
     C                   when      xxchar = 'D'
     C                   eval      kktier = '3'
     C*
     C                   endsl
     C*
     C                   eval      kkcode = durate
     C*
     C* (5) Get relative weight from FMMGP
     C*
     C     mgky          setgt     Pmg01
     C     mgky1         readpe    Pmg01
     C*
     C                   if        not %eof
     C                   eval      xxrelw = mgrelw
     C                   eval      xxalos = mgalos
     C                   endif
     C                   endif
     C*
     C* (6),(7),(8) Get rate values from FTRTP for 'MC'
     C*
     C                   Z-add     cafdat        kkefdt
     C                   Movel     'MC'          kkst
     C                   Movel     'R'           kktype
     C*
     C     rtky          setgt     prt01
     C     rtky1         readpe    prt01                                  99
     C*
     C                   dow       *in99 = *off
     C                   if        rtstat = 'I'
     C     rtky1         readpe    prt01                                  99
     C                   else
     C*
     C* (13) Rural Adjustement only if state code i.e cbsa is of 2 digits
     C*
     C                   if        xxlen  =  2
     C                   eval      xxruradj    = RTIRAD
     C                   else
     C                   eval      xxruradj = 0
     C                   endif
     C                   leave
     C*
     C                   endif
     C                   enddo
     C*
     C* (12) Teaching adjustments
     C*
     C                   eval(h)   xxtch_adj = (1 + ircime) ** 0.6876
     C*
     C* (14) Lip adjustments
     C*
     C***** Beg Chg *** 11/13/14 ********************************************
     C                   eval(h)   lip_adj = (1 + irssi + irmrat) ** RTLipPow
     C*****              eval(h)   lip_adj = (1 + irssi + irmrat) ** 0.4613
     C***** End Chg *** 11/13/14 ********************************************
     C*
     C                   endsr
    C*================================================================
    C*  SrCalc - Calculation for payment
    C*================================================================
     C     SRCalc        Begsr
     C*
     C* (16) Unadjusted Federal prospective payment
     C*
     C                   eval(h)   NA_fed = xxfed
     C*
     C* (17) Labor Portion of Federal payment
     C*
     C                   eval(h)   NA_F_lbp = NA_fed * rtilpc
     C*
     C* (18) Wage adjusted amount
     C*
     C                   eval(h)   W_A_amt = NA_F_lbp * iwwind
     C*
     C* (19) Non labor amount
     C*
     C                   eval(h)   NA_F_nlbp = NA_fed * rtinpc
     C*
     C* (20) Wage adjusted federal payment
     C*
     C                   eval(h)   W_A_fed = W_A_amt + NA_F_nlbp
     C*
     C* (21) rural adjsutment
     C*
     C                   eval      rur_adj = 1 + xxruradj
     C*
     C* (22) Wage and Rural Adjusted federal payment
     C*
     C                   eval(h)   W_R_A_fed = W_A_fed * rur_adj
     C                   eval(h)   @@irfb = W_R_A_fed
     C*
     C* (23) 2011 Wage, Rural and Lip Adjusted federal pros payment
     C*
     C                   eval(h)   W_R_L_A_fed = W_R_A_fed * lip_adj
     C*
     C* (24) LIP Adjusted amount
     C*
     C                   eval(h)   L_A_amt = W_R_A_fed * (lip_adj - 1)
     C                   eval      @@lip = L_A_amt
     C*
     C* (25) LIP Adjusted payment
     C*
     C                   eval(h)   L_A_pmt = W_R_A_fed + L_A_amt
     C*
     C* (26) Teaching Status Adjusted amount
     C*
     C                   eval(h)   T_S_A_amt = (W_R_A_fed * xxtch_adj) -
     C                                         W_R_A_fed
     C                   eval      @@tch = T_S_A_amt
     C*
     C* (28) 2011 adjsuted pros payment
     C*
     C                   eval(h)   adj_pros_pmt = W_R_L_A_fed + T_S_A_amt
     C*
     C                   endsr
    C*================================================================
    C*  SRoutl   - Outlier calculations
    C*================================================================
     C     SRoutl        Begsr
     C*
     C* (32) Labor portion of Outlier threshold
     C*
     C                   eval(h)   outl_lbp = rtoutl * rtilpc
     C*
     C* (33) Wage Adjusted amount
     C*
     C                   eval(h)   W_A_outl = outl_lbp * iwwind
     C*
     C* (34) Non labor amount
     C*
     C                   eval(h)   outl_nlbp = rtoutl - outl_lbp
     C*
     C* (35) Wage Adjusted outlier threshold amount
     C*
     C                   eval(h)   W_A_O_thrh = W_A_outl + outl_nlbp
     C*
     C* (37) Wage and Rural Adjusted outlier threshold amount
     C*
     C                   eval(h)   W_R_A_O_thrh = W_A_O_thrh * rur_adj
     C*
     C* (38) Wage, Rural and Lip Adjusted outlier threshold amount
     C*
     C                   eval(h)   W_R_L_A_O_thrh = W_R_A_O_thrh * lip_adj
     C*
     C* (39) Teaching adjustment amount
     C*
     C                   eval(h)   T_A_amt = (W_R_A_O_thrh * xxtch_adj)
     C                                       - W_R_A_O_thrh
     C*
     C* (40) Total Hypothetical Outlier threshold amount
     C*
     C                   eval(h)   H_O_amt = W_R_L_A_O_thrh + T_A_amt
     C*
     C* (43) Total Hypothetical outlier threshold
     C*
     C                   eval(h)   T_H_outl = H_O_amt + tot_adj
     C*
     C* (44) Estimation cost
     C*
     C                   eval(h)   O_ccr = cachrg * iroccr
     C*
     C* (44a) check for outlier applicability . If true, then calculate outlier
     C*
     C                   eval      @@outl = 0
     C                   eval      H_O_pmnt = 0
     C*
     C                   if        O_ccr > T_H_outl
     C*
     C* (45) Hypothetical Outlier Payment
     C*
     C                   eval(h)   H_O_pmnt = 0.80 * ( O_ccr - T_H_outl)
     C                   eval      @@outl = H_O_pmnt
     C                   endif
     C*
     C* (47) Total hypothertical payment ( PPS payment + outlier)
     C*
     C                   eval(h)   T_H_pmnt = H_O_pmnt + tot_adj
     C*
     C                   endsr
    C*================================================================
    C*  SrRegP - Calculations for regular payments
    C*================================================================
     C     SrRegP        Begsr
     C*
     C                   eval(h)   xxfed =  xxrelw * rtbncv
     C                   exsr      srcalc
     C*
     C                   eval(h)   tot_adj = adj_pros_pmt
     C                   exsr      sroutl
     C*
     C                   endsr
    C*================================================================
    C*  SrShortP - Calculations for short payments
    C*================================================================
     C     SrShortP      Begsr
     C*
     C* Override the code/weight for short stay conditions
     C*
     C                   select
     C*
     C* For days <= 3 (unconditional short stay case)
     C*
     C                   when      duunts <= 3
     C*
     C                   if        eupats = '20' or eupats = '40' or
     C                             eupats = '41' or eupats = '42'
     C                   eval      xxcode = 'A5101'
     C                   else
     C                   eval      xxcode = 'A5001'
     C                   endif
     C*
     C* Code for expired patient status and orthopedic case
     C*
     C                   when      (eupats = '20' or eupats = '40' or
     C                             eupats = '41' or eupats = '42') and
     C                             (dubrat = 'A0901' or dubrat = 'A0902' or
     C                              dubrat = 'A0903' or dubrat = 'A0904')
     C*
     C                   if        duunts <= 13
     C                   eval      xxcode = 'A5101'
     C                   elseif    duunts >= 14
     C                   eval      xxcode = 'A5102'
     C                   endif
     C*
     C* Code for expired patient status and non orthopedic case
     C*
     C                   when      (eupats = '20' or eupats = '40' or
     C                             eupats = '41' or eupats = '42') and
     C                             (dubrat <> 'A0901' and dubrat <> 'A0902'
     C                              and dubrat <> 'A0903' and dubrat <> 'A0904')
     C*
     C                   if        duunts <= 15
     C                   eval      xxcode = 'A5103'
     C                   elseif    duunts >= 16
     C                   eval      xxcode = 'A5104'
     C                   endif
     C*
     C                   endsl
     C*
     C* Get relative weight from FMMGP for the overriding code
     C*
     C                   if        xxcode <> *blanks
     C                   eval      kkcode = xxcode
     C                   eval      kktier = '0'
     C*
     C     mgky          setgt     Pmg01
     C     mgky1         readpe    Pmg01
     C*
     C                   if        not %eof
     C                   eval      xxrelw = mgrelw
     C                   eval      xxalos = mgalos
     C                   eval      @@code = xxcode
     C*
     C* Update Update FDDUP in case short stay loaded a different paid code
     C*
     C                   if        *in98 = *off
     C*
     C                   eval      durate = xxcode
     C                   eval      dudesc = mgdesc
    C*
     C                   move      q1user        duuser
     C                   move      q1pgm         dupgm
    C*
     C                   time                    dsdttm
    C*
     C                   move      dstime        dutime
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         dudt
    C*
     C                   update    pdu01
    C*
     C                   endif
    C*
     C                   endif
     C*
     C                   endif                                                  xxcode <> *blanks
     C*
     C                   eval(h)   xxfed =  xxrelw * rtbncv
     C                   exsr      srcalc
     C*
     C                   eval(h)   tot_adj = adj_pros_pmt
     C                   exsr      sroutl
     C*
     C                   endsr
    C*================================================================
    C*  SrTransP - Calculations for transfer payments
    C*================================================================
     C     SrTransP      Begsr
    C*
     C* (48) CMG Base Rate
     C*
     C                   eval(h)   CMG_brt = xxrelw * rtbncv
     C*
     C* (49) CMG Relative weight/los
     C*
     C                   eval(h)   CMG_rlw = xxrelw / xxalos
     C*
     C* (50) Per Diem rate for 1st day
     C*
     C                   eval(h)   per_diem_1 = (CMG_brt * CMG_rlw)
     C                   eval(h)   xxfed = per_diem_1
     C                   exsr      srcalc
     C                   eval      pay_diem_1  = 1.5 * adj_pros_pmt
     C*
     C* (55) Per Diem rate for remaining days
     C*
     C                   eval(h)   per_diem_rem = (CMG_brt * CMG_rlw)
     C                   eval(h)   xxfed = per_diem_rem
     C                   exsr      srcalc
     C                   eval      pay_diem_rem = adj_pros_pmt
     C*
     C* (56) Total adjusted payment
     C*
     C                   eval(h)   tot_adj = pay_diem_1 +
     C                             (pay_diem_rem * (duunts - 1))
     C* populate Federal adjustment for FDCAP
     C                   eval      @@irfb = (1.5 * W_R_A_fed) +
     C                                      (W_R_A_fed * (duunts - 1 ))
     C*
     C                   eval      @@lip = (1.5 * L_A_amt) +
     C                                      (L_A_amt * (duunts - 1 ))
     C* Add outlier if applicable
     C*
     C                   exsr      SrOutl
     C*
     C                   endsr
    C*================================================================
    C*  SrInit - Initialize variables
    C*================================================================
     C     SrInit        Begsr
     C*
     C                   eval      @@alw1 = 0
     C                   eval      @@alw2 = 0
     C                   eval      @@alw3 = 0
     C                   eval      xxlen = 0
     C                   eval      xxchar = *blanks
     C                   eval      xxrelw = 0
     C                   eval      xxalos = 0
     C                   eval      xxruradj = 0
     C                   eval      xxtch_adj = 0
     C                   eval      lip_adj = 0
     C                   eval      NA_fed = 0
     C                   eval      NA_F_lbp = 0
     C                   eval      W_A_amt = 0
     C                   eval      NA_F_nlbp = 0
     C                   eval      W_A_fed = 0
     C                   eval      rur_adj = 0
     C                   eval      W_R_A_fed = 0
     C                   eval      W_R_L_A_fed = 0
     C                   eval      L_A_amt = 0
     C                   eval      L_A_pmt = 0
     C                   eval      T_S_A_amt = 0
     C                   eval      adj_pros_pmt = 0
     C                   eval      tot_adj = 0
     C*
     C                   eval      outl_lbp = 0
     C                   eval      W_A_outl = 0
     C                   eval      outl_nlbp = 0
     C                   eval      W_A_O_thrh = 0
     C                   eval      W_R_A_O_thrh = 0
     C                   eval      W_R_L_A_O_thrh = 0
     C                   eval      T_A_amt = 0
     C                   eval      H_O_amt = 0
     C                   eval      T_H_outl = 0
     C                   eval      O_ccr = 0
     C                   eval      H_O_pmnt = 0
     C                   eval      T_H_pmnt = 0
     C*
     C                   eval      CMG_brt = 0
     C                   eval      CMG_rlw = 0
     C*
     C                   endsr
    C*================================================================
    C*  clsOverride - Close and Delter Override FTABL
    C*================================================================
     C     clsOverride   Begsr
     C*
     C                   close     ftabl
     C                   call      'QCMDEXC'                              99
     C                   parm                    qca(2)
     C                   parm      56            length
     C*
     C                   endsr
    C*================================================================
    C*  opnOverride - Open and Override FTABL
    C*================================================================
     C     opnOverride   Begsr
     C*
     C                   eval      length = 56
     C                   movel(p)  qca(1)        qqcmd
     C                   movel(p)  eoast         qqst
     c*
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm      56            length           15 5
     C                   open      ftabl
     C*
     C                   endsr
**  (QCMDEX)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
DLTOVR FILE(FTABL)
