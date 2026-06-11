    H*****************************************************************
     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H*****************************************************************
    H* Update PA,NJ, CK EOB X-ref
    H**
    H*****************************************************************
    H* Created: 04/06/18 BY C:79 For 114MJC
    H*****************************************************************
     Fftebpnj1  if   e             disk    rename(peb01:pebnj2)
     Ffteblnj   uf   e           k disk    rename(peb01:pebnj1)
     Fftebppa1  if   e             disk    rename(peb01:pebpa2)
     Ffteblpa   uf   e           k disk    rename(peb01:pebpa1)
     Fftebpck1  if   e             disk    rename(peb01:pebck2)
     Ffteblck   uf   e           k disk    rename(peb01:pebck1)
    D*
     D psds          esds                  extname(szq1p)
    D*
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D*
     D getCo#          pr                  extpgm('FA007R')
     D ##co#                          3  0
     D ##fnd                          3  0
     D ##conm                        40
     D ##fnam                        40
     D ##co#           s              3  0
     D ##fnd           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D kkco#           s                   like(ebco#)
     D kkkey           s                   like(ebkey)
     D xxxref          s                   like(ebxref)
     D xxxref2         s                   like(ebxref)
     D xxdt            s              7  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C     ebky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
      /Free
                         callp getCo#(##co#:##fnd:##conm:##fnam);
                         kkco# = ##co#;
      /End-Free
     C                   time                    dsdttm
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         xxdt
    C*
    C* New Jersey EOB Code Cross Reference Codes
    C*
     C                   read      pebnj2                                 99
    C*
     C                   dow       *in99 = *off
    C*
     C                   eval      kkkey = ebkey
     C                   eval      xxxref= ebxref
     C                   eval      xxxref2 = ebxrf2
    C*
     C                   if        xxxref <> *blanks or xxxref2 <> *blanks
    C*
     C     ebky          chain     pebnj1                             99
    C*
     C                   if        *in99 = *off
     C*
     C                   eval      ebxref= xxxref
     C                   eval      ebxrf2= xxxref2
     C*
     C                   eval      ebpgm = q1pgm
     C                   eval      ebuser= q1user
     C                   eval      ebdt  = xxdt
     C                   eval      ebtime= dstime
     C*
     C                   update    pebnj1
     C*
     C                   endif
    C*
     C                   endif
    C*
     C                   read      pebnj2                                 99
    C*
     C                   enddo
    C*
    C* Pennsylvania EOB Code Cross Reference Codes
    C*
     C                   read      pebpa2                                 99
    C*
     C                   dow       *in99 = *off
    C*
     C                   eval      kkkey = ebkey
     C                   eval      xxxref= ebxref
     C                   eval      xxxref2 = ebxrf2
    C*
     C                   if        xxxref <> *blanks or xxxref2 <> *blanks
    C*
     C     ebky          chain     pebpa1                             99
    C*
     C                   if        *in99 = *off
     C*
     C                   eval      ebxref= xxxref
     C                   eval      ebxrf2= xxxref2
     C*
     C                   eval      ebpgm = q1pgm
     C                   eval      ebuser= q1user
     C                   eval      ebdt  = xxdt
     C                   eval      ebtime= dstime
     C*
     C                   update    pebpa1
     C*
     C                   endif
    C*
     C                   endif
    C*
     C                   read      pebpa2                                 99
    C*
     C                   enddo
    C*
    C* ClaimCheck EOB Code Cross Reference Codes
    C*
     C                   read      pebck2                                 99
    C*
     C                   dow       *in99 = *off
    C*
     C                   eval      kkkey = ebkey
     C                   eval      xxxref= ebxref
     C                   eval      xxxref2 = ebxrf2
    C*
     C                   if        xxxref <> *blanks or xxxref2 <> *blanks
    C*
     C     ebky          chain     pebck1                             99
    C*
     C                   if        *in99 = *off
     C*
     C                   eval      ebxref= xxxref
     C                   eval      ebxrf2= xxxref2
     C*
     C                   eval      ebpgm = q1pgm
     C                   eval      ebuser= q1user
     C                   eval      ebdt  = xxdt
     C                   eval      ebtime= dstime
     C*
     C                   update    pebck1
     C*
     C                   endif
    C*
     C                   endif
    C*
     C                   read      pebck2                                 99
    C*
     C                   enddo
    C*
     C                   move      *on           *inlr
    C*
     C                   return
