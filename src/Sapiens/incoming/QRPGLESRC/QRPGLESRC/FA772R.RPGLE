     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H*  CREATED BY                                                   *
     H*                                                               *
     H*  10/06/11  C:251 Project: 114MZO                              *
     H*                                                               *
     H*  This program will calculate the SNF allowance and will       *
     H*  return 'F' if the records in files is not found and  will    *
     H*  return a '*' if the data is insufficiant to calculate the    *
     H*  the allowance                                                *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  03/14/14  C:274 Project: FS427                               *
     H*                  Strip off leading "999" coming from          *
     H*                  FMSKP/KKCBSA                                 *
     H*                                                               *
     H*****************************************************************
     Ffmskl1    if   e           k disk
     Ffmhpl1    if   e           k disk
     Ffmrgl1    if   e           k disk
     Ffmwgl2    if   e           k disk
     Ffdcap     if   e             disk
     Ffddul     if   e           k disk
     Ffaeul     if   e           k disk
     Ffaeol4    if   e           k disk
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
     D*** Beg Add 03/14/14 *******************************************
     D                 ds
     D  xxCBSA                 1      5
     D  xxCBSA3                1      3
     D  xxCBSA2                4      5
     D*** End Add 03/14/14 *******************************************
     D*
     D*
     Dxxlbp_pmt        s              9  2
     Dxxsnf_pmt        s              9  2
     Dxxtot_pmt        s              9  2
     Dxxtot            s              9  2
     DNonLabor         s              9  2
     DLabor            s              9  2
     DLabor_adj        s              9  2
     Drural_urban      s              1
     Dxxlen            s              5  0
     Dxxcbs            s              2
     Dkkcar            s                   like(skprov)
     Dxxunts           s                   like(duunts)
     Dxxwind           s                   like(wgwind)
     Dkkcat            s                   like(rgcat)
     D*
     C     *entry        plist
     C                   parm                    @@rrn             9 0           RRN of FDCAP
     C                   parm                    @@call            1
     C                   parm                    @@alw1            9 2
     C                   parm                    @@alw2            9 2
     C                   parm                    @@alw3            9 2
     C                   parm                    @@rtcd            1
     C*
     C     dtcymd1       plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C*
     C     skky          klist
     C                   kfld                    kkcar
     C                   kfld                    kkffdt
     C*
     C     skky1         klist
     C                   kfld                    kkcar
     C*
     C     rgky          klist
     C                   kfld                    kkcat
     C                   kfld                    kkefdt
     C*
     C     rgky1         klist
     C                   kfld                    kkcat
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
     C     wgky          klist
     C                   kfld                    kkcbsa
     C                   kfld                    kkefdt
      *
     C     wgky1         klist
     C                   kfld                    kkcbsa
      *
     C*
     C     *like         define    eoco#         kkco#
     C     *like         define    eosub#        kksub#
     C     *like         define    eofnd         kkfnd
     C     *like         define    eofyr         kkfyr
     C     *like         define    eocase        kkcase
     C     *like         define    eoeob#        kkeob#
     C     *like         define    skffdt        kkffdt
     C     *like         define    skacbs        kkcbsa
     *
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
B001 C                   if        *in99 = *on
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
     C*** Initialising
     C*
     C                   eval      @@alw1 = 0
     C                   eval      @@alw2 = 0
     C                   eval      @@alw3 = 0
     C                   eval      xxlbp_pmt = 0
     C                   eval      xxsnf_pmt = 0
     C                   eval      xxtot_pmt = 0
     C                   eval      labor = 0
     C                   eval      nonlabor = 0
     C                   eval      labor_adj = 0
     C                   eval      xxtot  = 0
     C                   eval      xxwind = 0
     C                   eval      xxlen  = 0
     C                   eval      xxunts = 0
     C                   eval      kkcar  = *blanks
     C                   eval      kkcbsa = *blanks
     C                   eval      rural_urban = *blanks
     C*** Beg Add 03/14/14 ********************************************
     C                   Eval      xxCBSA = *blanks
     C*** End Add 03/14/14 ********************************************
     C*
     C                   Z-add     euscpt        kkefdt
     C*
     C*** Get rate values from FTRTP for 'MC'
     C*
     C                   Movel     'MC'          kkst
     C                   Movel     'S'           kktype
     C*
     C     rtky          setgt     prt01
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
     C*** Get CBSA from FMSKP
     C*
     C                   move      euscpt        srdti
     C                   call      'SRCVTDAT'    dtcymd1
     C                   Movel     srdto         kkffdt
     C*
     C                   eval      kkcar = eumcar
     C*
     C     skky          setgt     psk01                              99
     C     skky1         readpe    psk01                                  99
     C                   if        *in99 = *off
     C*** Beg Add 03/14/14 ********************************************
     C                   Eval      xxCBSA = skacbs
     C                   if        xxCBSA3 = '999'
     C                   eval      xxCBSA3 = *blanks
     C                   eval      skacbs  = xxCBSA
     C                   endif
     C*** End Add 03/14/14 ********************************************
     C                   eval      xxlen = %len(%trim(skacbs))
     C*
     C                   if        skacbs  = '00000' or skacbs = *blanks
     C                   eval      xxlen = 0
     C                   endif
     C*
     C                   if        xxlen > 2
     C                   Eval      rural_urban  = 'U'
     C                   movel     SKACBS        kkCBSA
     C                   else
     C                   Eval      rural_urban  = 'R'
     C                   move      skacbs        xxcbs
     C                   movel     xxcbs         kkcbsa
     C                   endif
     C                   endif
     C*
     C*** Get the field WGWIND
     C*
     C     wgky          setgt     Pwg01
     C     wgky1         readpe    Pwg01                                  99
     C                   if        *in99 = *off
     C                   eval      xxwind = wgwind
     C                   endif                                                  wgky
     C*
     C*** Get cpt4 code from FDDUP
     C*
     C     duky          chain     pdu01                              99
     C                   if        *in99 = *off
     C*** Get medicare days
     C*
     C                   eval      xxunts = duunts
     C*
     C*** Get RUG code (first three chars of HCPCS code from file FDDUP)
     C*
     C                   eval      kkcat = *blanks
     C                   eval      kkcat = %subst(durate:1:3)
     C*
     C*** Get allowance from FMRGP
     C*
     C     rgky          setgt     Prg01
     C     rgky1         readpe    Prg01                                  99
     C                   if        *in99 = *off
     C*
     C                   select
     C                   when      rural_urban = 'U'
     C                   eval      xxtot = rgutot
     C                   when      rural_urban = 'R'
     C                   eval      xxtot = rgrtot
     C                   endsl
     C*
     C                   endif
     C                   endif
     C*
     C*** Calculate Labor share
     C                   eval(h)   labor = xxtot * rtsnfl
     C*
     C*** Calculate wage index adjusted Labor share
     C                   eval(h)   labor_adj = labor * xxwind
     C*
     C*** Calculate non-Labor share
     C                   eval(h)   nonlabor = xxtot * (1 - rtsnfl)
     C*
     C*** Calculate total amount
     C                   eval(h)   xxlbp_pmt = labor_adj + nonlabor
     C*
     C*** Calculate total SNF payment
     C                   eval(h)   xxsnf_pmt = xxlbp_pmt * xxunts
     C*
     C*** Pass back SNF payment to calling program
     C                   eval      @@alw1 =  xxsnf_pmt
     C*
     C                   seton                                        LR
     C                   return
    C*================================================================
    C*  clsOverride - Close and Delter Override FTABL
    C*================================================================
     C     clsOverride   begsr
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
     C     opnOverride   begsr
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
